(in-package #:cl-telegram-bot-auto-api)

(serapeum:eval-always
  (defvar *tg-api-json-url* "https://raw.githubusercontent.com/rockneurotiko/telegram_api_json/master/exports/tg_api.json")
  (defvar *tg-api-json-pathname* (asdf:system-relative-pathname "cl-telegram-bot-auto-api" "telegram_api_json/tg_api.json"))
  (defvar *tg-api*
    (njson:decode (or (ignore-errors (dex:get *tg-api-json-url*))
                      *tg-api-json-pathname*)))
  (defvar *types* nil)
  (defvar *parents* nil))

(defmacro define-tg-apis ()
  (let ((api (njson:decode (or (ignore-errors (dex:get *tg-api-json-url*))
                               *tg-api-json-pathname*)))
        (types (serapeum:dict 'equal
                              "int" 'integer
                              "float" 'float
                              "str" 'string
                              "file" 'pathname
                              "bool" 't
                              "array" 'sequence))
        (parents (serapeum:dict)))
    (labels ((json->name (json-name)
               (alexandria:symbolicate
                (cond
                  ((some #'upper-case-p json-name)
                   (cl-json:simplified-camel-case-to-lisp json-name))
                  ((find #\_ json-name :test #'eql)
                   (string-upcase (substitute #\- #\_ json-name)))
                  (t
                   (cl-json:simplified-camel-case-to-lisp json-name)))))
             (type-name (string)
               (let ((string (if (listp string) (first string) string)))
                 (or (gethash string types)
                     (json->name string))))
             (define-generics (json)
               (loop for generic in json
                     for name = (json->name (njson:jget "name" generic))
                     do (setf (gethash (njson:jget "name" generic) types)
                              name)
                     do (dolist (subtype (njson:jget "subtypes" generic))
                          (setf (gethash (json->name subtype) parents)
                                name))
                     collect `(serapeum:export-always (quote ,name))
                     collect `(defclass ,name () ())))
             (define-classes (json)
               (loop for class in json
                     for class-name
                       = (json->name (njson:jget "name" class))
                     do (setf (gethash (njson:jget "name" class) types)
                              class-name)
                     collect `(serapeum:export-always (quote ,class-name))
                     collect (let ((class-name class-name))
                               `(defclass ,class-name (,@(when (gethash class-name parents)
                                                           (list (gethash class-name parents))))
                                  (,@(loop for param in (njson:jget "params" class)
                                           for name = (json->name (njson:jget "name" param))
                                           collect `(,(json->name (njson:jget "name" param))
                                                     :accessor ,(json->name (njson:jget "name" param))
                                                     ,@(alexandria:when-let ((types (mapcar #'type-name (njson:jget "type" param))))
                                                         `(:type (or ,@types)))
                                                     :documentation ,(njson:jget "description" param))))
                                  (:documentation ,(njson:jget "description" class))))
                     append (loop for param in (njson:jget "params" class)
                                  collect `(serapeum:export-always
                                               (quote ,(json->name (njson:jget "name" param)))))))
             (define-methods (json)
               (loop for method in json
                     for method-name
                       = (json->name (njson:jget "name" method))
                     for required-args
                       = (remove-if (alexandria:curry #'njson:jget "optional")
                                    (njson:jget "params" method))
                     for optional-args
                       = (set-difference (njson:jget "params" method) required-args)
                     collect `(serapeum:export-always (quote ,method-name))
                     collect `(defgeneric ,method-name
                                  (,@(loop for arg in required-args
                                           collect (json->name (njson:jget "name" arg)))
                                   ,@(when optional-args
                                       (append '(&rest args &key)
                                               (loop for arg in optional-args
                                                     collect (json->name (njson:jget "name" arg)))
                                               '(&allow-other-keys))))
                                ,@(labels ((type-combinations (types)
                                             (if (rest types)
                                                 (loop for type in (first types)
                                                       append (loop for ending in (type-combinations (rest types))
                                                                    collect (cons type ending)))
                                                 (mapcar #'list (first types)))))
                                    (let ((combinations (type-combinations
                                                         (mapcar (lambda (arg)
                                                                   (mapcar #'type-name (njson:jget "type" arg)))
                                                                 required-args))))
                                      (if combinations
                                          (loop for combination in combinations
                                                collect `(:method (,@(loop for arg in required-args
                                                                           for type in combination
                                                                           collect (list (json->name (njson:jget "name" arg))
                                                                                         type))
                                                                   ,@(when optional-args
                                                                       (append '(&rest args &key)
                                                                               (loop for arg in optional-args
                                                                                     collect (json->name (njson:jget "name" arg)))
                                                                               '(&allow-other-keys))))))
                                          `((:method (,@(when optional-args
                                                          (append '(&rest args &key)
                                                                  (loop for arg in optional-args
                                                                        collect (json->name (njson:jget "name" arg)))
                                                                  '(&allow-other-keys)))))))))
                                  (:documentation ,(njson:jget "description" method))))))
      (prog1
          `(progn
             ,@(define-generics (njson:jget "generics" api))
             ,@(define-classes (njson:jget "models" api))
             ,@(define-methods (njson:jget "methods" api)))
        (setf *types* types
              *parents* parents)))))

(define-tg-apis)
