(in-package :cl-user)
(defpackage cjhunt-asd
  (:use :cl :asdf))
(in-package :cjhunt-asd)

(defsystem cjhunt
  :version "0.1"
  :author "Adlai Chandrasekhar"
  :license "do what thou wilt"
  :depends-on (:clack
               :lack
               :caveman2
               :envy
               :cl-ppcre
               :uiop

               ;; for @route annotation
               :cl-syntax-annot

               ;; HTML Template
               :djula

               ;; for DB
               :datafly                 ; well, someday
               :sxql

               ;; for cjhunt
               :split-sequence :decimals
               :cl-json :parse-float
               :alexandria :anaphora
               :local-time :drakma
               :fare-memoization        ; well, dumbsay
               )
  :components ((:module "src"
                :components
                ((:file "util") (:file "config" :depends-on ("util"))
                 (:file "db" :depends-on ("config"))
                 (:module "bitcoin" :depends-on ("config") :serial t
                  :components ((:file "rpc-client") (:file "api")
                               (:file "raw-parser")))
                 (:file "hunt" :depends-on ("bitcoin"))
                 (:file "view" :depends-on ("config"))
                 (:file "web" :depends-on ("view" "bitcoin" "hunt"))
                 (:file "main" :depends-on ("config" "view" "db")))))
  :description "coinjoin hunter"
  :in-order-to ((test-op (load-op cjhunt-test))))
