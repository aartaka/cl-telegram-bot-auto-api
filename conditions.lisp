(in-package #:cl-telegram-bot-auto-api)

(serapeum:export-always 'telegram-error)
(define-condition telegram-error (error)
  ((description :initarg :description
                :accessor description))
  (:report (lambda (condition stream)
             (format stream "Telegram error:
~a"
                     (description condition)))))

(serapeum:export-always 'unimplemented)
(define-condition unimplemented (error)
  ((specifier :initarg :specifier
              :accessor specifier))
  (:report (lambda (condition stream)
             (format stream "ON not implemented for ~a"
                     (specifier condition)))))
