(in-package :cl-user)
(defpackage cjhunt-asd
  (:use :cl :asdf))
(in-package :cjhunt-asd)

(defsystem cjhunt
  :version "0.1"
  :author "Adlai Chandrasekhar"
  :license "do what thou wilt"
  :depends-on (:clack
               :caveman2
               :envy
               :cl-ppcre

               ;; for @route annotation
               :cl-syntax-annot

               ;; HTML Template
               :djula

               ;; for DB
               :datafly
               :sxql

               ;; for cjhunt
               :cl-json
               :drakma
               :parse-float
               :alexandria
               :local-time
               )
  :components ((:module "src"
                :components
                ((:file "main" :depends-on ("config" "view" "db"))
                 (:file "web" :depends-on ("view" "hunt"))
                 (:file "view" :depends-on ("config"))
                 (:file "db" :depends-on ("config"))
                 (:file "config") (:file "hunt"))))
  :description "coinjoin hunter"
  :in-order-to ((test-op (load-op cjhunt-test))))
