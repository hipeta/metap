;;;; metap.asd
;;;;  
;;;; Copyright 2015 hipeta (rhipeta@gmail.com)
;;;;
;;;; This software is released under the MIT License.
;;;; http://opensource.org/licenses/mit-license.php

(asdf:defsystem #:metap
  :author "hipeta"
  :license "MIT"
  :description "Metap provides metaclass propagation along with class inheritance structures."
  :serial t
  :depends-on (:alexandria
               :closer-mop
               :cl-thread-macro)
  :components ((:file "metap")))

