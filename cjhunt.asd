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
                 (:file "bitcoin-rpc" :depends-on ("config"))
                 (:file "hunt" :depends-on ("bitcoin-rpc"))
                 (:file "view" :depends-on ("config"))
                 (:file "web" :depends-on ("view" "bitcoin-rpc" "hunt"))
                 (:file "main" :depends-on ("config" "view" "db")))))
  :description "coinjoin hunter"
  :in-order-to ((test-op (load-op cjhunt-test))))
