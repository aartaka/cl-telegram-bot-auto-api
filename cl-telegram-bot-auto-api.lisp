(in-package #:cl-telegram-bot-auto-api)

(serapeum:eval-always
  (export '(*tg-api-json-pathname* *tg-api-json-url* *api-url* *token*))
  (defvar *tg-api-json-pathname*
    (asdf:system-relative-pathname "cl-telegram-bot-auto-api" "telegram_api_json/exports/tg_api.json")
    "The pathname to find API JSON file at.")
  (defvar *tg-api-json-url*
    "https://raw.githubusercontent.com/rockneurotiko/telegram_api_json/master/exports/tg_api.json"
    "The URL to fetch the API JSON file from in case it's not found locally.")
  (defvar *types* (serapeum:dict 'equal
                                 "int" 'integer
                                 "float" 'float
                                 "str" 'string
                                 "file" 'pathname
                                 "bool" 't
                                 "array" 'sequence)
    "The table with all the types TG-AUTO-API has, from the Telegram name to Lisp type.")
  (defvar *parents* (serapeum:dict)
    "The hash table from the subclasses to their generic classes.")
  (defvar *api-url* "https://api.telegram.org/"
    "The base URL to send bot methods to.
Bot token and method name is appended to it.")
  (defvar *token* nil "Telegram bot token. Bound per bot thread."))

(defun parse-as (class-symbol object)
  (etypecase object
    (hash-table
     (apply #'make-instance
            class-symbol
            (loop for (key . value) in (alexandria:hash-table-alist  object)
                  collect (alexandria:make-keyword (string-upcase (substitute #\- #\_ key)))
                  collect value)))
    (sequence (map 'list (alexandria:curry #'parse-as class-symbol)
                   object))
    (t (unless (typep (find-class class-symbol nil) 'standard-class)
         object))))

(serapeum:eval-always
  (export 'telegram-object)
  (defclass telegram-object () ())
  (defgeneric unparse (object)
    (:method ((object t))
      object)
    (:method ((object telegram-object))
      (loop for slot in (mapcar #'closer-mop:slot-definition-name
                                (closer-mop:class-slots (class-of object)))
            when (slot-boundp object slot)
              collect (cons (string-downcase (substitute #\_ #\- (symbol-name slot)))
                            (unparse (funcall slot object)))))
    (:documentation "Transform the object into an NJSON-friendly alist of literal values when necessary.")))

(defun invoke-method (method-name &rest args &key &allow-other-keys)
  (let ((return (njson:decode
                 (handler-case
                     (apply #'dex:post
                            (quri:render-uri (quri:make-uri :path (uiop:strcat "bot" *token* "/" method-name)
                                                            :defaults *api-url*))
                            :headers '(("Content-Type" . "application/json"))
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
  (defun json->name (json-name)
    (intern
     (cond
       ((some #'upper-case-p json-name)
        (cl-json:simplified-camel-case-to-lisp json-name))
       ((find #\_ json-name :test #'eql)
        (string-upcase (substitute #\- #\_ json-name)))
       (t
        (cl-json:simplified-camel-case-to-lisp json-name)))
     :tga))
  (defun type-name (type)
    "Returns two values:
- Primitive `parse-as'-friendly type, preferably atomic.  If the TYPE
  is a mere \"array\" without element type, then, well, returns the
  corresponding Lisp array type.
- The outermost type. Same as the first value, unless array TYPE."
    (cond
     ((listp type)
      (let* ((inner-type (labels ((inner-type (type)
                                    (cond
                                      ((and (listp type)
                                            (equal "array" (first type)))
                                       (inner-type (caadr type)))
                                      ((listp type)
                                       (first type))
                                      (t type))))
                           (inner-type type)))
             (inner-type (or (gethash inner-type *types*)
                             (json->name inner-type))))
        (values
         inner-type
         (if (equal "array" (first type))
             'sequence
             inner-type))))
     (t (let ((type (or (gethash type *types*)
                        (json->name type))))
          (values type type)))))
  (defun define-generics (json)
    (loop for generic in json
          for name = (json->name (njson:jget "name" generic))
          do (setf (gethash (njson:jget "name" generic) *types*)
                   name)
          do (dolist (subtype (njson:jget "subtypes" generic))
               (setf (gethash (json->name subtype) *parents*)
                     name))
          collect `(serapeum:export-always (quote ,name))
          collect `(defclass ,name (telegram-object) ())))
  (defun define-classes (json)
    (loop for class in json
          for class-name
            = (json->name (njson:jget "name" class))
          do (setf (gethash (njson:jget "name" class) *types*)
                   class-name)
          collect `(serapeum:export-always (quote ,class-name))
          collect (let ((class-name class-name))
                    `(defclass ,class-name (,@(if (gethash class-name *parents*)
                                                  (list (gethash class-name *parents*))
                                                  (list 'telegram-object)))
                       (,@(loop for param in (njson:jget "params" class)
                                for name = (json->name (njson:jget "name" param))
                                collect `(,(json->name (njson:jget "name" param))
                                          :initarg ,(alexandria:make-keyword (json->name (njson:jget "name" param)))
                                          :documentation ,(njson:jget "description" param))))
                       (:documentation ,(njson:jget "description" class))))
          append (loop for param in (njson:jget "params" class)
                       collect `(serapeum:export-always
                                    (quote ,(json->name (njson:jget "name" param)))))
          append (let ((class-name class-name))
                   (loop for param in (njson:jget "params" class)
                         for name = (json->name (njson:jget "name" param))
                         collect `(defgeneric ,name (object)
                                    (:generic-function-class telegram-method))
                         collect `(defmethod ,name ((object ,class-name))
                                    (slot-value object (quote ,name)))
                         collect `(defmethod ,name :around ((object ,class-name))
                                    (handler-case
                                        (values
                                         ,(alexandria:if-let
                                              ((type (set-difference (mapcar #'type-name (njson:jget "type" param))
                                                                     '(integer float string pathname t nil sequence))))
                                            `(parse-as (quote ,(first type)) (call-next-method))
                                            `(call-next-method))
                                         t)
                                      (unbound-slot ()
                                        (values nil nil))))))))
  (defun define-methods (json)
    (loop for method in json
          for params = (njson:jget "params" method)
          for method-name
            = (json->name (njson:jget "name" method))
          for required-args
            = (remove-if (alexandria:curry #'njson:jget "optional")
                         params)
          for required-arg-names
            = (loop for arg in required-args
                    collect (json->name (njson:jget "name" arg)))
          for optional-args
            = (set-difference params required-args)
          for optional-arg-names
            = (loop for arg in optional-args
                    collect (json->name (njson:jget "name" arg)))
          collect `(serapeum:export-always (quote ,method-name))
          collect `(defgeneric ,method-name
                       (,@required-arg-names
                        ,@(when optional-args
                            (append '(&rest args &key)
                                    optional-arg-names
                                    '(&allow-other-keys))))
                     (:generic-function-class telegram-method)
                     (:documentation ,(apply
                                       #'concatenate
                                       'string
                                       (njson:jget "description" method)
                                       (string #\newline)
                                       (mapcar
                                        (lambda (p)
                                          (format nil "~:@(~a~) -- ~a~&"
                                                  (substitute #\- #\_ (njson:jget "name" p))
                                                  (njson:jget "description" p)))
                                        params))))
          append (labels ((type-combinations (types)
                            (if (rest types)
                                (loop for type in (first types)
                                      append (loop for ending in (type-combinations (rest types))
                                                   collect (cons type ending)))
                                (mapcar #'list (first types))))
                          (method-body (method required-arg-names rest-args?)
                            `(let ((result
                                     (apply
                                      #'invoke-method
                                      ,(njson:jget "name" method)
                                      (append
                                       (list ,@(loop for name in required-arg-names
                                                     append (list (alexandria:make-keyword name)
                                                                  name)))
                                       ,(if rest-args? 'args '())))))
                               ,(if (equal '("true") (njson:jget "return" method))
                                    'result
                                    `(parse-as (quote ,(type-name (njson:jget "return" method)))
                                               result)))))
                   (let ((combinations (type-combinations
                                        (mapcar (lambda (arg)
                                                  (mapcar (lambda (type) (nth-value 1 (type-name type)))
                                                          (njson:jget "type" arg)))
                                                required-args))))
                     (if combinations
                         (loop for combination in combinations
                               collect `(defmethod ,method-name (,@(loop for name in required-arg-names
                                                                         for type in combination
                                                                         collect (list name type))
                                                                 ,@(when optional-args
                                                                     (append '(&rest args &key)
                                                                             optional-arg-names
                                                                             '(&allow-other-keys))))
                                          (declare (ignorable ,@required-arg-names ,@optional-arg-names))
                                          ,(method-body method required-arg-names optional-args)))
                         `((defmethod ,method-name (,@(when optional-args
                                                        (append '(&rest args &key)
                                                                optional-arg-names
                                                                '(&allow-other-keys))))
                             (declare (ignorable ,@optional-arg-names))
                             ,(method-body method required-arg-names optional-args))))))))
  (defmacro define-tg-apis ()
    (let ((api (njson:decode (or *tg-api-json-pathname*
                                 (ignore-errors (dex:get *tg-api-json-url*))))))
      `(progn
         ,@(define-generics (njson:jget "generics" api))
         ,@(define-classes (njson:jget "models" api))
         ,@(define-methods (njson:jget "methods" api))))))

(define-tg-apis)

;; NOTE: Exported already, no need to `serapeum:export-always'.
(defgeneric id (object)
  (:method ((update update))
    (update-id update))
  (:method ((result chosen-inline-result))
    (result-id result))
  (:method ((message message))
    (message-id message))
  (:method ((message message-id))
    (message-id message))
  (:generic-function-class telegram-method)
  (:documentation "Universal ID-fetching method for all types of objects.
Even those not having ID property in Telegram API."))

(defmethod no-applicable-method ((fn telegram-method) &rest args)
  (let* ((new-required (loop for arg in args
                             until (keywordp arg)
                             if (find-method #'id '() (list (class-of arg)) nil)
                               collect (id arg)
                             else collect arg))
         (specifiers (loop for req in new-required
                           if (subtypep (type-of req) 'standard-object)
                             collect (class-of req)
                           else collect (find-class (first (uiop:ensure-list (type-of req))) nil)))
         (new-key (nthcdr (or (position-if #'keywordp args) (cl:length args))
                          args))
         (new-arglist (append new-required new-key)))
    (when (find-method fn '() specifiers nil)
      (apply fn new-arglist))))

(serapeum:export-always 'on)
(defgeneric on (object)
  (:method ((object null))
    nil)
  (:method ((object telegram-object))
    (cerror "Ignore unimplemented method."
            'unimplemented
            :specifier (class-of object)))
  (:method ((update update))
    (dolist (slot (remove 'update-id
                          (mapcar #'closer-mop:slot-definition-name
                                  (closer-mop:class-slots (class-of update)))))
      (on (funcall slot update))))
  (:method :after ((message message))
    (dolist (entity (entities message))
      (on entity)))
  (:documentation "The universal method to call on event objects Telegram gives.
Default method only defined for `update', other methods throw `unimplemented' error."))

(serapeum:export-always 'start)
(defmethod start (token &key name update-callback error-callback (timeout 10))
  "Start the bot designated by the provided TOKEN and return the thread processing happens on.

You can start several bots with this, and they will work just fine on
their own threads (with the exception of `on' methods being shared
between those due to CLOS single-threaded-ness). You can stop any of
those with, e.g., `bt:destroy-thread'.

Process the updates and their contents with `on' or UPDATE-CALLBACK, if provided.
On error, call either `on' or ERROR-CALLBACK (if provided) with the error as the sole argument.

NAME is used to name the thread for bot update processing.
TIMEOUT is passed to `get-updates'."
  (macrolet ((with-protect (error-callback &body body)
               `(handler-bind ((error ,(or error-callback #'on)))
                  ,@body)))
    (bt:make-thread
     (lambda ()
       (loop with last-id = nil
             while t
             for updates = (with-protect error-callback
                             (apply #'get-updates
                                    :timeout timeout
                                    (when last-id
                                      (list :offset last-id))))
             when updates
               do (with-protect error-callback
                    (mapc (or update-callback #'on) updates))
             do (setf last-id (1+ (reduce #'max updates :key #'update-id :initial-value 0)))
             do (sleep 1)))
     :initial-bindings `((*token* . ,token))
     :name (if name
               (uiop:strcat "Telegram bot '" name "' thread")
               "Telegram bot thread"))))
