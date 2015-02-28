;;;; metap-test.lisp
;;;;  
;;;; Copyright 2015 hipeta (rhipeta@gmail.com)
;;;;
;;;; This software is released under the MIT License.
;;;; http://opensource.org/licenses/mit-license.php

(in-package :cl-user)
(defpackage metap-test
  (:use :cl :fiveam :metap))
(in-package :metap-test)

(def-suite all)
(in-suite all)

(defclass a () ())
(defclass b (a) ())
(defclass c (a) ())
(defclass d (b c) ())
(defclass e (a) ())
(defclass f (d e) ())
(defclass g (d) ())
(defclass h (g e) ())
(defclass i (f) ())
(defclass j (f) ())
(defclass k (h i j) ())

(test compute-precedense-list-test
  (is (equal (metap::compute-precedense-list (mapcar #'find-class '(h i j)))
             (cdr (c2mop:compute-class-precedence-list (find-class 'k))))))
