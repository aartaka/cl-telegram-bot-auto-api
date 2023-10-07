;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3 Clause

;;; Commentary:
;;
;; GNU Guix development package.  To build and install, clone this repository,
;; switch directory to here and run:
;;
;;   guix package --install-from-file=guix.scm
;;
;; To use as the basis for a development environment, run:
;;
;;   guix shell --container -D -f guix.scm
;;
;; Replace --container by --pure if you still want ASDF to see external
;; libraries in ~/common-lisp, etc.
;; To build a local executable and then run it:
;;; Code:

(use-modules (guix packages)
             ((guix licenses) #:prefix license:)
             (guix gexp)
             (guix git-download)
             (guix build-system asdf)
             (gnu packages)
             (gnu packages lisp)
             (gnu packages lisp-check)
             (gnu packages lisp-xyz))

(define-public sbcl-telegram-bot-auto-api
  (package
   (name "sbcl-telegram-bot-auto-api")
   (version "0.1.1")
   (source
    (local-file (dirname (current-filename)) #:recursive? #t)
    ;;;; Or this, in case of contributing to Guix.
    ;; (origin
    ;;   (method git-fetch)
    ;;   (uri (git-reference
    ;;         (url "https://github.com/aartaka/cl-telegram-bot-auto-api")
    ;;         (recursive? #t)
    ;;         (commit version)))
    ;;   (file-name (git-file-name "cl-telegram-bot-auto-api" version))
    ;;   (sha256
    ;;    (base32
    ;;     "SPECIFY-HASH")))
    )
   (build-system asdf-build-system/sbcl)
   ;; We use `cl-*' inputs and not `sbcl-*' ones so that CCL users can also use
   ;; this Guix manifests.
   ;;
   ;; Another reason is to not fail when an input dependency is found in
   ;; ~/common-lisp, which would trigger a rebuild of the SBCL input in the
   ;; store, which is read-only and would thus fail.
   ;;
   ;; The official Guix package should use `sbcl-*' inputs though.
   (native-inputs (list cl-lisp-unit2 sbcl))
   (inputs (list
            cl-dexador
            cl-quri
            cl-njson
            cl-bordeaux-threads
            cl-alexandria
            cl-serapeum))
   (arguments
    '(#:asd-systems '("cl-telegram-bot-auto-api")))
   (synopsis "Auto-generated Common Lisp API for Telegram Bots.")
   (home-page "https://github.com/aartaka/cl-telegram-bot-auto-api")
   (description "This library aims to make Telegram bots writing easy.
Easy on the part of making sure all the bindings are available and
up-to-date. It stems from the problem one has with (otherwise perfect)
@code{cl-telegram-bot}—one has to add lots of methods to even make
one's bot ideas tested.

This library solves the problem of having to
write Telegram Bot API bindings by hand—all the classes and methods
are generated automatically at load-time from @code{telegram_api_json}
JSON files.\n")
   (license license:bsd-2)))

(define-public cl-telegram-bot-auto-api
  (sbcl-package->cl-source-package sbcl-telegram-bot-auto-api))

(define-public ecl-telegram-bot-auto-api
  (sbcl-package->ecl-package sbcl-telegram-bot-auto-api))

cl-telegram-bot-auto-api
