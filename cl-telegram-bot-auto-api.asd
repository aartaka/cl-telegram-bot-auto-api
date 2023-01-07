(asdf:defsystem "cl-telegram-bot-auto-api"
  :description "Auto-generated Common Lisp API for Telegram Bots."
  :author "Artyom Bologov"
  :license  "BSD 2-Clause"
  :version "0.0.1"
  :serial t
  :depends-on ("dexador" "quri" "njson/cl-json" "bordeaux-threads" "alexandria" "serapeum")
  :components ((:file "package")
               (:file "cl-telegram-bot-auto-api")))
