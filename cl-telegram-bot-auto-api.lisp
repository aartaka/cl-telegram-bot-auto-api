(in-package #:cl-telegram-bot-auto-api)

(serapeum:eval-always
  (export '(*tg-api-json-pathname* *tg-api-json-url* *api-url* *token*))
  (defvar *tg-api-json-pathname*
    (asdf:system-relative-pathname "cl-telegram-bot-auto-api" "telegram_api_json/exports/tg_api.json")
    "The pathname to find API JSON file at")
  (defvar *tg-api-json-url*
    "https://raw.githubusercontent.com/rockneurotiko/telegram_api_json/master/exports/tg_api.json"
    "The URL to fetch the API JSON file from in case it's not found locally")
  (defvar *types* (serapeum:dict 'equal
                                 "int" 'integer
                                 "float" 'float
                                 "str" 'string
                                 "file" 'pathname
                                 "bool" 't
                                 "array" 'sequence)
    "The table with all the types TG-AUTO-API has, from the Telegram name to Lisp type")
  (defvar *parents* (serapeum:dict)
    "The hash table from the subclasses to their generic classes")
  (defvar *api-url* "https://api.telegram.org/"
    "The base URL to send bot methods to.
Bot token and method name is appended to it")
  (defvar *token* nil "Telegram bot token. Bound per bot thread")
  (defvar *timeout* 10
    "The timeout for telegram requests and waiting on response. Bound per bot thread."))

(defun parse-as (class-symbol object)
  (etypecase object
    (hash-table
     (closer-mop:finalize-inheritance (find-class class-symbol))
     (apply #'make-instance
            class-symbol
            (loop with slots = (closer-mop:class-slots (find-class class-symbol))
                  for (key . value) in (alexandria:hash-table-alist  object)
                  for name = (string-upcase (substitute #\- #\_ key))
                  for slot = (find name slots
                                   :test #'string-equal :key #'closer-mop:slot-definition-name)
                  collect (alexandria:make-keyword name)
                  if (subtypep (closer-mop:slot-definition-type slot) 'telegram-object)
                    collect (parse-as (closer-mop:slot-definition-type slot) value)
                  else
                    collect value)))
    (sequence (map 'list (alexandria:curry #'parse-as class-symbol)
                   object))
    (t object)))

(serapeum:eval-always
  (export 'telegram-object)
  (defclass telegram-object () ())
  (defgeneric unparse (object)
    (:method ((object t))
      object)
    (:method ((object string))
      object)
    (:method ((object sequence))
      (map 'vector #'unparse object))
    (:method ((object telegram-object))
      (loop for slot in (mapcar #'closer-mop:slot-definition-name
                                (closer-mop:class-slots (class-of object)))
            when (slot-boundp object slot)
              collect (cons (string-downcase (substitute #\_ #\- (symbol-name slot)))
                            (unparse (funcall slot object)))))
    (:documentation "Transform the object into an NJSON-friendly alist of literal values when necessary")))

(defmethod print-object ((object telegram-object) stream)
  (print-unreadable-object (object stream :type t)
    (when (ignore-errors (id object))
      (cl:format stream "~a=~a" 'id (id object)))
    (dolist (slot (mapcar #'closer-mop:slot-definition-name
                          (closer-mop:class-slots (class-of object))))
      (when (slot-boundp object slot)
        (cl:format stream " ~a=~a" slot (funcall slot object))))))

(defun invoke-method (method-name &rest args &key &allow-other-keys)
  (let ((return (njson:decode
                 (handler-case
                     (apply #'dex:post
                            (quri:render-uri (quri:make-uri :path (uiop:strcat "bot" *token* "/" method-name)
                                                            :defaults *api-url*))
                            :headers '(("Content-Type" . "application/json"))
                            :read-timeout *timeout*
                            :connect-timeout *timeout*
                            (when args
                              (list :content
                                    (njson:encode
                                     (loop for (key . value) in (alexandria:plist-alist args)
                                           collect (cons (string-downcase (substitute #\_ #\- (symbol-name key)))
                                                         (unparse value)))))))
                   (dex:http-request-failed (e)
                     (dex:response-body e))))))
    (if (ignore-errors (njson:jget "ok" return))
        (njson:jget "result" return)
        (cerror "Ignore this error"
                'telegram-error :description (njson:jget "description" return)))))

(serapeum:eval-always
  (defclass telegram-method (standard-generic-function)
    ()
    (:metaclass closer-mop:funcallable-standard-class))
  (defun json->name (json-name &optional (package :tga))
    (intern
     (cond
       ((some #'upper-case-p json-name)
        (cl-json:simplified-camel-case-to-lisp json-name))
       ((find #\_ json-name :test #'eql)
        (string-upcase (substitute #\- #\_ json-name)))
       (t
        (cl-json:simplified-camel-case-to-lisp json-name)))
     package))
  (defun type-name (type)
    "Return two values:
- Primitive `parse-as'-friendly type, preferably atomic.  If the TYPE
  is a mere \"array\" without element type, then, well, returns the
  corresponding Lisp array type.
- The outermost type. Same as the first value, unless array TYPE."
    (cond
     ((arrayp type)
      (let* ((inner-type (labels ((inner-type (type)
                                    (cond
                                      ((and (arrayp type)
					    (not (stringp type))
                                            (equal "array" (elt type 0)))
                                       (inner-type (elt (elt type 1) 0)))
                                      ((and (arrayp type)
					    (not (stringp type)))
                                       (elt type 0))
                                      (t type))))
                           (inner-type type)))
             (inner-type (or (gethash inner-type *types*)
                             (json->name inner-type))))
        (values
         inner-type
         (if (equal "array" (elt type 0))
             'sequence
             inner-type))))
     (t (let ((type (or (gethash type *types*)
                        (json->name type))))
          (values type type)))))
  (defun define-generics (generics)
    (loop for generic across generics
          for name = (json->name (njson:jget "name" generic))
          do (setf (gethash (njson:jget "name" generic) *types*)
                   name)
          do (loop for subtype across (njson:jget "subtypes" generic)
		   do (setf (gethash (json->name subtype) *parents*)
			    name))
          collect `(serapeum:export-always ',name)
          collect `(defclass ,name (telegram-object) ())))
  (defun define-classes (classes)
    (loop for class across classes
          for class-name
            = (json->name (njson:jget "name" class))
          do (setf (gethash (njson:jget "name" class) *types*)
                   class-name)
          collect `(serapeum:export-always ',class-name)
          collect (let ((class-name class-name))
                    `(defclass ,class-name (,@(if (gethash class-name *parents*)
                                                  (list (gethash class-name *parents*))
                                                  (list 'telegram-object)))
                       (,@(loop for param across (njson:jget "params" class)
                                for name = (json->name (njson:jget "name" param))
                                collect `(,(json->name (njson:jget "name" param))
                                          :initarg ,(alexandria:make-keyword (json->name (njson:jget "name" param)))
                                          :type ,(if (serapeum:single (njson:jget "type" param))
                                                     (nth-value 1 (type-name (elt (njson:jget "type" param) 0)))
                                                     `(or ,@(map 'list (lambda (type)
									 (nth-value 1 (type-name type)))
								 (njson:jget "type" param))))
                                          :documentation ,(njson:jget "description" param))))
                       (:documentation ,(njson:jget "description" class))))
          append (loop for param across (njson:jget "params" class)
                       collect `(serapeum:export-always
                                    ',(json->name (njson:jget "name" param))))
          append (let ((class-name class-name))
                   (loop for param across (njson:jget "params" class)
                         for name = (json->name (njson:jget "name" param))
                         collect `(defgeneric ,name (object)
                                    (:generic-function-class telegram-method))
                         collect `(defmethod ,name ((object ,class-name))
                                    (slot-value object ',name))
                         collect `(defmethod ,name :around ((object ,class-name))
                                    (handler-case
                                        (values
                                         ,(alexandria:if-let
                                              ((type (set-difference (map 'list #'type-name (njson:jget "type" param))
                                                                     '(integer float string pathname t nil sequence))))
                                            `(parse-as ',(elt type 0) (call-next-method))
                                            `(call-next-method))
                                         t)
                                      (unbound-slot ()
                                        (values nil nil))))))))
  (defun define-methods (methods)
    (loop for method across methods
          for params = (njson:jget "params" method)
          for method-name
            = (json->name (njson:jget "name" method))
          for required-args
            = (remove-if (alexandria:curry #'njson:jget "optional")
                         params)
          for required-arg-names
            = (loop for arg across required-args
                    collect (json->name (njson:jget "name" arg)))
          for optional-args
            = (remove-if (lambda (p) (find p required-args))
			 params)
          for optional-arg-names
            = (loop for arg across optional-args
                    collect (json->name (njson:jget "name" arg)))
          collect `(serapeum:export-always ',method-name)
          collect `(defgeneric ,method-name
                       (,@required-arg-names
                        ,@(when (plusp (cl:length optional-args))
			    (append '(&rest args &key)
				    optional-arg-names
				    '(&allow-other-keys))))
                     (:generic-function-class telegram-method)
                     (:documentation ,(apply
                                       #'concatenate
                                       'string
                                       (njson:jget "description" method)
                                       (string #\newline)
                                       (map 'list
					    (lambda (p)
					      (cl:format nil "~:@(~a~) -- ~a~&"
                                                         (substitute #\- #\_ (njson:jget "name" p))
                                                         (njson:jget "description" p)))
					    params))))
          append (labels ((type-combinations (types)
			    (cond
			      ((> (cl:length types) 1)
			       (loop for type in (elt types 0)
				     append (loop for ending in (type-combinations (subseq types 1))
						  collect (cons type ending))))
			      ((= (cl:length types) 1)
			       (map 'list #'list (elt types 0)))))
                          (method-body (method required-arg-names rest-args?)
                            `(let ((result
                                     (apply
                                      #'invoke-method
                                      ,(njson:jget "name" method)
                                      (append
                                       (list ,@(loop for name in required-arg-names
                                                     append (list (alexandria:make-keyword name)
                                                                  name)))
                                       ,(when rest-args? 'args)))))
                               ,(if (equalp #("true") (njson:jget "return" method))
                                    'result
                                    `(parse-as ',(type-name (elt (njson:jget "return" method) 0))
                                               result)))))
                   (let ((combinations (type-combinations
                                        (map 'list (lambda (arg)
						     (map 'list (lambda (type) (nth-value 1 (type-name type)))
                                                          (njson:jget "type" arg)))
					     required-args))))
                     (if combinations
                         (loop for combination in combinations
                               collect `(defmethod ,method-name (,@(loop for name in required-arg-names
                                                                         for type in combination
                                                                         collect (list name type))
                                                                 ,@(when (plusp (cl:length optional-args))
                                                                     (append '(&rest args &key)
                                                                             optional-arg-names
                                                                             '(&allow-other-keys))))
                                          (declare (ignorable ,@required-arg-names ,@optional-arg-names))
                                          ,(method-body method required-arg-names (plusp (cl:length optional-args)))))
                         `((defmethod ,method-name (,@(when (plusp (cl:length optional-args))
                                                        (append '(&rest args &key)
                                                                optional-arg-names
                                                                '(&allow-other-keys))))
                             (declare (ignorable ,@optional-arg-names))
                             ,(method-body method required-arg-names (plusp (cl:length optional-args))))))))))
  (defmacro define-tg-apis ()
    (let ((api (njson:decode (or *tg-api-json-pathname*
                                 (ignore-errors (dex:get *tg-api-json-url*))))))
      `(progn
         ,@(define-generics (njson:jget "generics" api))
         ,@(define-classes (njson:jget "models" api))
         ,@(define-methods (njson:jget "methods" api))))))

(define-tg-apis)

;; NOTE: Exported already, no need to `serapeum:export-always'.
(defmethod id ((update update))
  (update-id update))
(defmethod id ((result chosen-inline-result))
  (result-id result))
(defmethod id ((message message))
  (message-id message))
(defmethod id ((message message-id))
  (message-id message))

;; NOTE: Exported already, no need to `serapeum:export-always'.
(defmethod command ((update update))
  "Return:
- Command name, as a Lisp keyword,
- Text of the message after the command, with the leading spaces stripped off."
  (when (message update)
    (command (message update))))
(defmethod command ((message message))
  "Return:
- Command name, as a Lisp keyword,
- Text of the message after the command, with the leading spaces stripped off."
  (dolist (entity (entities message))
    (with-accessors ((offset offset) (length length) (type type))
        entity
      (when (equal "bot_command" type)
        (let ((command (subseq (text message) (1+ offset) (+ offset length))))
          (return-from command
            (values
             (json->name
              (alexandria:if-let ((at-pos (cl:position #\@ command :test #'char=)))
                (subseq command 0 at-pos)
                command)
              :keyword)
             (uiop:strcat (subseq (text message) 0 offset)
                          (string-trim serapeum:whitespace (subseq (text message) (+ offset length)))))))))))

(defvar *applicable-method-depth* 0)

(defmethod no-applicable-method ((fn telegram-method) &rest args)
  (if (zerop *applicable-method-depth*)
      (let* ((new-required (loop for arg in args
                                 until (keywordp arg)
                                 if (find-method #'id '() (list (class-of arg)) nil)
                                   collect (id arg)
                                 else collect arg))
             (new-key (nthcdr (or (position-if #'keywordp args) (cl:length args))
                              args))
             (new-arglist (append new-required new-key)))
        ;; FIXME: This is reckless, but `find-method' seems to misbehave
        ;; much too often...
        (let ((*applicable-method-depth* (1+ *applicable-method-depth*)))
          (apply fn new-arglist)))
      (cerror "Ignore it"
              "Cannot find any applicable method for function ~a with arguments ~a" fn args)))

(serapeum:export-always 'on)
(defgeneric on (object)
  (:method ((object null))
    nil)
  (:method ((object telegram-object))
    (cerror "Ignore unimplemented method."
            'unimplemented
            :specifier (class-of object)))
  (:method ((object condition))
    (princ object)
    (uiop:print-backtrace :condition object))
  (:method ((update update))
    (dolist (slot (remove 'update-id
                          (mapcar #'closer-mop:slot-definition-name
                                  (closer-mop:class-slots (class-of update)))))
      (on (funcall slot update))))
  (:documentation "The universal method to call on event objects Telegram gives
Default method only defined for `update', other methods throw `unimplemented' error."))

(serapeum:export-always 'start)
(defmethod start (token &key name update-callback error-callback (timeout *timeout*))
  "Start the bot designated by the provided TOKEN and return the thread processing happens on

You can start several bots with this, and they will work just fine on
their own threads (with the exception of `on' methods being shared
between those due to CLOS single-threaded-ness). You can stop any of
those with, e.g., `bt:destroy-thread'.

Process the updates and their contents with `on' or UPDATE-CALLBACK, if provided.
On error, call either `on' or ERROR-CALLBACK (if provided) with the error as the sole argument.

NAME is used to name the thread for bot update processing.
TIMEOUT is passed to `get-updates'."
  (bt:make-thread
   (lambda ()
     (loop with last-id = nil
           while t
           for updates = (handler-bind ((error #'(lambda (e)
                                                   (funcall (or error-callback #'on) e))))
                           (apply #'get-updates
                                  :timeout timeout
                                  (when last-id
                                    (list :offset last-id))))
           when updates
             do (handler-bind ((error #'(lambda (e)
                                          (funcall (or error-callback #'on) e))))
                  (mapc (or update-callback #'on) updates))
           do (setf last-id (1+ (reduce #'max updates :key #'update-id :initial-value 0)))
           do (sleep 1)))
   :initial-bindings `((*token* . ,token)
                       (*timeout* . ,timeout))
   :name (if name
             (uiop:strcat "Telegram bot '" name "' thread")
             "Telegram bot thread")))
