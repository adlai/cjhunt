(in-package :cl-user)
(defpackage cjhunt-test-asd
  (:use :cl :asdf))
(in-package :cjhunt-test-asd)

(defsystem cjhunt-test
  :author "Adlai Chandrasekhar"
  :license "do what thou wilt"
  :depends-on (:cjhunt
               :prove)
  :components ((:module "t"
                :components
                ((:file "cjhunt"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
