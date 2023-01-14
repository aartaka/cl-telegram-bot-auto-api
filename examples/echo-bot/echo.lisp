(in-package :echo-bot)

(defmethod tga:on ((message tga:message))
  "Respond to the message with the same text it contains (actually using `tga:copy-message')."
  (tga:copy-message (tga:chat message) (tga:chat message) message))

(defmethod tga:on ((object error))
  "Invoke CONTINUE restart of OBJECT, if found."
  (when (find-restart 'continue object)
    (invoke-restart 'continue)))

(defun entry-point (&key (token (first uiop:*command-line-arguments*)))
  (tga:start token :name "Echo bot"))
