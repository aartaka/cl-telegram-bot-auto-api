(in-package #:cl-telegram-bot-auto-api)

(defvar *tg-api-json-url* "https://raw.githubusercontent.com/rockneurotiko/telegram_api_json/master/exports/tg_api.json")
(defvar *tg-api-json-pathname* (asdf:system-relative-pathname "cl-telegram-bot-auto-api" "telegram_api_json/tg_api.json"))
(defvar *tg-api* (njson:decode (or (ignore-errors (dex:get *tg-api-json-url*))
                                   *tg-api-json-pathname*)))

(defvar *types* nil)
(defvar *parents* nil)

(defmacro define-tg-apis ()
  (let ((api (njson:decode (or (ignore-errors (dex:get *tg-api-json-url*))
                               *tg-api-json-pathname*)))
        (types (serapeum:dict 'equalp
                              "int" 'integer
                              "float" 'float
                              "string" 'string
                              "file" 'pathname
                              "bool" 'boolean
                              "array" 'sequence))
        (parents (serapeum:dict)))
    (labels ((json->name (json-name &key prefix)
               (alexandria:symbolicate
                (or prefix "")
                (if prefix
                    "-"
                    "")
                (cond
                  ((some #'upper-case-p json-name)
                   (cl-json:simplified-camel-case-to-lisp json-name))
                  ((find #\_ json-name :test #'eql)
                   (string-upcase (substitute #\- #\_ json-name)))
                  (t
                   (cl-json:simplified-camel-case-to-lisp json-name)))))
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
                                                     :accessor ,(json->name (njson:jget "name" param) :prefix class-name)
                                                     :documentation ,(njson:jget "description" param))))
                                  (:documentation ,(njson:jget "description" class))))
                     append (loop for param in (njson:jget "params" class)
                                  collect `(serapeum:export-always
                                               (quote ,(json->name (njson:jget "name" param) :prefix class-name))))))
             (define-methods (json)
               '()))
      `(progn
         ,@(define-generics (njson:jget "generics" api))
         ,@(define-classes (njson:jget "models" api))
         ,@(define-methods (njson:jget "methods" api))
         ,@(progn
             (setf *types* types
                   *parents* parents)
             nil)))))

(define-tg-apis)
