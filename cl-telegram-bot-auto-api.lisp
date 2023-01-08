(in-package #:cl-telegram-bot-auto-api)

(serapeum:eval-always
  (defvar *tg-api-json-url* "https://raw.githubusercontent.com/rockneurotiko/telegram_api_json/master/exports/tg_api.json")
  (defvar *tg-api-json-pathname* (asdf:system-relative-pathname "cl-telegram-bot-auto-api" "telegram_api_json/exports/tg_api.json"))
  (defvar *types* (serapeum:dict 'equal
                                 "int" 'integer
                                 "float" 'float
                                 "str" 'string
                                 "file" 'pathname
                                 "bool" 't
                                 "array" 'sequence))
  (defvar *parents* (serapeum:dict))
  (defvar *api-url* "https://api.telegram.org/")
  (defvar *token* nil))

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

(serapeum:export-always 'telegram-error)
(define-condition telegram-error (error)
  ((description :initarg :description
                :accessor description))
  (:report (lambda (condition stream)
             (format stream "Telegram error:
~a"
                     (description condition)))))

(defgeneric unparse (object)
  (:method ((object t))
    object)
  (:method ((object telegram-object))
    (loop for slot in (mapcar #'closer-mop:slot-definition-name
                              (closer-mop:class-slots (class-of object)))
          when (slot-boundp object slot)
            collect (cons (string-downcase (substitute #\_ #\- (symbol-name slot)))
                          (unparse (slot-value object slot)))))
  (:documentation "Transform the object into an NJSON-friendly alist of literal values when necessary."))

(defun invoke-method (method-name &rest args &key &allow-other-keys)
  (let ((return (njson:decode
                 (apply #'dex:post
                        (quri:render-uri (quri:make-uri :path (uiop:strcat "bot" *token* "/" method-name)
                                                        :defaults *api-url*))
                        :headers '(("Content-Type" . "application/json"))
                        (when args
                          (list :content
                                (njson:encode
                                 (loop for (key . value) in (alexandria:plist-alist args)
                                       collect (cons (string-downcase (substitute #\_ #\- (symbol-name key)))
                                                     (unparse value))))))))))
    (if (njson:jget "ok" return)
        (njson:jget "result" return)
        (cerror "Ignore this error"
                'telegram-error :description (njson:jget "description" return)))))

(serapeum:eval-always
  (defclass telegram-object () ())
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
                                          :accessor ,(json->name (njson:jget "name" param))
                                          :initarg ,(alexandria:make-keyword (json->name (njson:jget "name" param)))
                                          :documentation ,(njson:jget "description" param))))
                       (:documentation ,(njson:jget "description" class))))
          append (loop for param in (njson:jget "params" class)
                       collect `(serapeum:export-always
                                    (quote ,(json->name (njson:jget "name" param)))))
          append (let ((class-name class-name))
                   (loop for param in (njson:jget "params" class)
                         for name = (json->name (njson:jget "name" param))
                         collect `(defmethod ,name :around
                                      ((object ,class-name))
                                    (setf (slot-value object (quote ,name))
                                          ,(alexandria:if-let
                                               ((type (set-difference (mapcar #'type-name (njson:jget "type" param))
                                                                      '(integer float string pathname t nil sequence))))
                                             `(parse-as (quote ,(first type)) (call-next-method))
                                             `(call-next-method))))))))
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
                     ,@(labels ((type-combinations (types)
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
                                     collect `(:method (,@(loop for name in required-arg-names
                                                                for type in combination
                                                                collect (list name type))
                                                        ,@(when optional-args
                                                            (append '(&rest args &key)
                                                                    optional-arg-names
                                                                    '(&allow-other-keys))))
                                                (declare (ignorable ,@required-arg-names ,@optional-arg-names))
                                                ,(method-body method required-arg-names optional-args)))
                               `((:method (,@(when optional-args
                                               (append '(&rest args &key)
                                                       optional-arg-names
                                                       '(&allow-other-keys))))
                                   (declare (ignorable ,@optional-arg-names))
                                   ,(method-body method required-arg-names optional-args))))))
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
                                        params))))))
  (defmacro define-tg-apis ()
    (let ((api (njson:decode (or *tg-api-json-pathname*
                                 (ignore-errors (dex:get *tg-api-json-url*))))))
      `(progn
         ,@(define-generics (njson:jget "generics" api))
         ,@(define-classes (njson:jget "models" api))
         ,@(define-methods (njson:jget "methods" api))))))

(define-tg-apis)

(macrolet ((d (name)
             `(progn
                (serapeum:export-always (quote ,name))
                (defgeneric ,name (object)))))
  (d on-message) (d on-edited-message)
  (d on-channel-post) (d on-edited-channel-post)
  (d on-inline-query) (d on-chosen-inline-result) (d on-callback-query)
  (d on-shipping-query) (d on-pre-checkout-query)
  (d on-poll) (d on-poll-answer)
  (d on-chat-member) (d on-my-chat-member) (d on-chat-join-request))

(serapeum:export-always 'on-update)
(defgeneric on-update (update)
  (:method ((update update))
    (macrolet ((when-apply (function slot)
                 (alexandria:once-only ((value `(when (slot-boundp update (quote ,slot))
                                                  (,slot update))))
                   `(when ,value
                      (,function ,value)))))
      (when-apply on-message message)
      (when-apply on-edited-message edited-message)
      (when-apply on-channel-post channel-post)
      (when-apply on-edited-channel-post edited-channel-post)
      (when-apply on-inline-query inline-query)
      (when-apply on-chosen-inline-result chosen-inline-result)
      (when-apply on-callback-query callback-query)
      (when-apply on-shipping-query shipping-query)
      (when-apply on-pre-checkout-query pre-checkout-query)
      (when-apply on-poll poll)
      (when-apply on-poll-answer poll-answer)
      (when-apply on-chat-member chat-member)
      (when-apply on-my-chat-member my-chat-member)
      (when-apply on-chat-join-request chat-join-request)))
  (:documentation "Process the parts of the update (if present), be it message, chat join request, or whatever."))

(serapeum:export-always '(start stop))
(defvar *thread* nil)
(let (*thread*)
  (defun start (token &key update-callback (timeout 10))
    (setf *thread*
          (bt:make-thread
           (lambda ()
             (loop with last-id = nil
                   while t
                   for updates = (apply #'get-updates
                                        :timeout timeout
                                        (when last-id
                                          (list :offset last-id)))
                   do (setf last-id (1+ (reduce #'max updates :key #'update-id :initial-value 0)))
                   when updates
                     do (map nil (or update-callback #'on-update) updates)))
           :initial-bindings `((*token* . ,token)
                               (*thread* . *thread*))
           :name "Telegram bot thread")))
  (defun stop ()
    (bt:destroy-thread *thread*)))
