(asdf:defsystem "echo-bot"
  :description "Echo bot using cl-telegram-bot-auto-api."
  :author "Artyom Bologov"
  :license  "BSD 2-Clause"
  :version "0.0.1"
  :serial t
  :build-operation "program-op"
  :build-pathname "echo"
  :entry-point "echo-bot:entry-point"
  :depends-on ("cl-telegram-bot-auto-api")
  :components ((:file "package")
               (:file "echo")))
