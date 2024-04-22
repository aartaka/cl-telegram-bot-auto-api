(asdf:defsystem "cl-telegram-bot-auto-api"
  :description "Auto-generated Common Lisp API for Telegram Bots."
  :author "Artyom Bologov"
  :homepage "https://github.com/aartaka/cl-telegram-bot-auto-api"
  :bug-tracker "https://github.com/aartaka/cl-telegram-bot-auto-api/issues"
  :source-control (:git "https://github.com/aartaka/cl-telegram-bot-auto-api.git")
  :license  "BSD 2-Clause"
  :version "0.1.1"
  :serial t
  :depends-on ("dexador" "quri" "njson/cl-json" "bordeaux-threads" "alexandria" "serapeum")
  :components ((:file "package")
               (:file "conditions")
               (:file "cl-telegram-bot-auto-api")
               (:module "JSON"
                :pathname "telegram_api_json"
                :components
                ((:static-file "exports/tg_api.json")
                 (:static-file "exports/tg_api_pretty.json")))))
