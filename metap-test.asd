;;;; metap-test.asd
;;;;  
;;;; Copyright 2015 hipeta (rhipeta@gmail.com)
;;;;
;;;; This software is released under the MIT License.
;;;; http://opensource.org/licenses/mit-license.php

(in-package :cl-user)
(defpackage metap-test-asd
  (:use :cl :asdf))
(in-package :metap-test-asd)

(defsystem metap-test
  :serial t
  :author "hipeta"
  :license "MIT"
  :description "metap test"
  :depends-on (:fiveam :metap)
  :components ((:file "metap-test")))

