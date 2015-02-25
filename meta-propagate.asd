;;;; meta-propagate.asd
;;;;  
;;;; Copyright 2015 hipeta (rhipeta@gmail.com)
;;;;
;;;; This software is released under the MIT License.
;;;; http://opensource.org/licenses/mit-license.php

(asdf:defsystem #:meta-propagate
  :serial t
  :description "meta-propagate provides metaclass propagation along with class inheritance structures."
  :author "hipeta"
  :license "MIT"
  :depends-on (:alexandria
               :closer-mop)
  :components ((:file "meta-propagate")))

