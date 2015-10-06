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
                ((:file "config") (:file "db" :depends-on ("config"))
                 (:module "bitcoin" :depends-on ("config") :serial t
                          :components ((:file "rpc-client") (:file "api")))
                 (:file "hunt" :depends-on ("bitcoin"))
                 (:file "view" :depends-on ("config"))
                 (:file "web" :depends-on ("view" "bitcoin" "hunt"))
                 (:file "main" :depends-on ("config" "view" "db")))))
  :description "coinjoin hunter"
  :in-order-to ((test-op (load-op cjhunt-test))))
