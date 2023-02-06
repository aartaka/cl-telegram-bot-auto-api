(in-package :echo-bot)

(defmethod tga:on ((message tga:message))
  "Respond to the message with the same text it contains (actually using `tga:copy-message')."
  (tga:copy-message (tga:chat message) (tga:chat message) message))

(defmethod tga:on ((object error))
  "Invoke CONTINUE restart of OBJECT, if found.
Otherwise print the backtrace and return."
  (or (continue)
      (progn
        (format t "~a" object)
        (uiop:print-backtrace :condition object))))

(defun entry-point (&key (token (first uiop:*command-line-arguments*)))
  (tga:start token :name "Echo bot"))
