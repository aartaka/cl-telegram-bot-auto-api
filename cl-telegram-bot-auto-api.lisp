(in-package #:cl-telegram-bot-auto-api)

(serapeum:eval-always
  (defvar *tg-api-json-url* "https://raw.githubusercontent.com/rockneurotiko/telegram_api_json/master/exports/tg_api.json")
  (defvar *tg-api-json-pathname* (asdf:system-relative-pathname "cl-telegram-bot-auto-api" "telegram_api_json/tg_api.json"))
  (defvar *tg-api*
    (njson:decode (or (ignore-errors (dex:get *tg-api-json-url*))
                      *tg-api-json-pathname*)))
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
                                                     value)))))))))
    (if (njson:jget "ok" return)
        (njson:jget "result" return)
        (error (njson:jget "description" return)))))

(serapeum:eval-always
  (defun json->name (json-name)
    (alexandria:symbolicate
     (cond
       ((some #'upper-case-p json-name)
        (cl-json:simplified-camel-case-to-lisp json-name))
       ((find #\_ json-name :test #'eql)
        (string-upcase (substitute #\- #\_ json-name)))
       (t
        (cl-json:simplified-camel-case-to-lisp json-name)))))
  (defun type-name (string)
    (let ((type (cond
                  ((listp string)
                   (first string))
                  (t string)))
          (array? (and (listp string)
                       (equal "array" (first string)))))
      (values (or (gethash type *types*)
                  (json->name type))
              (when array? t)
              (when array? (type-name (first (second string)))))))
  (defun define-generics (json)
    (loop for generic in json
          for name = (json->name (njson:jget "name" generic))
          do (setf (gethash (njson:jget "name" generic) *types*)
                   name)
          do (dolist (subtype (njson:jget "subtypes" generic))
               (setf (gethash (json->name subtype) *parents*)
                     name))
          collect `(serapeum:export-always (quote ,name))
          collect `(defclass ,name () ())))
  (defun define-classes (json)
    (loop for class in json
          for class-name
            = (json->name (njson:jget "name" class))
          do (setf (gethash (njson:jget "name" class) *types*)
                   class-name)
          collect `(serapeum:export-always (quote ,class-name))
          collect (let ((class-name class-name))
                    `(defclass ,class-name (,@(when (gethash class-name *parents*)
                                                (list (gethash class-name *parents*))))
                       (,@(loop for param in (njson:jget "params" class)
                                for name = (json->name (njson:jget "name" param))
                                collect `(,(json->name (njson:jget "name" param))
                                          :accessor ,(json->name (njson:jget "name" param))
                                          :initarg ,(alexandria:make-keyword (json->name (njson:jget "name" param)))
                                          ,@(alexandria:when-let ((types (mapcar #'type-name (njson:jget "type" param))))
                                              `(:type (or ,@types)))
                                          :documentation ,(njson:jget "description" param))))
                       (:documentation ,(njson:jget "description" class))))
          append (loop for param in (njson:jget "params" class)
                       collect `(serapeum:export-always
                                    (quote ,(json->name (njson:jget "name" param)))))))
  (defun define-methods (json)
    (loop for method in json
          for method-name
            = (json->name (njson:jget "name" method))
          for required-args
            = (remove-if (alexandria:curry #'njson:jget "optional")
                         (njson:jget "params" method))
          for required-arg-names
            = (loop for arg in required-args
                    collect (json->name (njson:jget "name" arg)))
          for optional-args
            = (set-difference (njson:jget "params" method) required-args)
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
                                (method-body (method required-arg-names)
                                  `(let ((result
                                           (apply
                                            #'invoke-method
                                            ,(njson:jget "name" method)
                                            (append
                                             (list ,@(loop for name in required-arg-names
                                                           append (list (alexandria:make-keyword name)
                                                                        name)))
                                             args))))
                                     ,(if (equal '("true") (njson:jget "return" method))
                                          'result
                                          `(parse-as (quote ,(multiple-value-bind (type array? array-type)
                                                                 (type-name (njson:jget "return" method))
                                                               (if array?
                                                                   array-type
                                                                   type)))
                                                     result)))))
                         (let ((combinations (type-combinations
                                              (mapcar (lambda (arg)
                                                        (mapcar #'type-name (njson:jget "type" arg)))
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
                                                ,(method-body method required-arg-names)))
                               `((:method (,@(when optional-args
                                               (append '(&rest args &key)
                                                       optional-arg-names
                                                       '(&allow-other-keys))))
                                   (declare (ignorable ,@optional-arg-names))
                                   ,(method-body method required-arg-names))))))
                     (:documentation ,(njson:jget "description" method)))))
  (defmacro define-tg-apis ()
    (let ((api (njson:decode (or (ignore-errors (dex:get *tg-api-json-url*))
                                 *tg-api-json-pathname*))))
      `(progn
         ,@(define-generics (njson:jget "generics" api))
         ,@(define-classes (njson:jget "models" api))
         ,@(define-methods (njson:jget "methods" api))))))

(define-tg-apis)
