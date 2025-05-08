(in-package #:cl-telegram-bot-auto-api)

(serapeum:export-always 'telegram-error)
(define-condition telegram-error (error)
  ((description :initarg :description
                :accessor telegram-error-description))
  (:report (lambda (condition stream)
             (cl:format stream "Telegram error:
~a"
                        (telegram-error-description condition)))))

(serapeum:export-always 'unimplemented)
(define-condition unimplemented (error)
  ((specifier :initarg :specifier
              :accessor unimplemented-specifier))
  (:report (lambda (condition stream)
             (cl:format stream "ON not implemented for ~a"
                        (unimplemented-specifier condition)))))
