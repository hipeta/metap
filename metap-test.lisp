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

(in-suite* all)

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

(defclass meta1 (standard-class) ())
(defclass test1-mixin () ())
(defmethod c2mop:validate-superclass ((c meta1) (s standard-class)) t)
(defmethod make-instance ((class meta1) &key)
  (format t "meta1")
  (call-next-method))

(defclass meta2 (meta1) ())
(defclass test2-mixin () ())
(defmethod c2mop:validate-superclass ((c meta2) (s meta1)) t)
(defmethod make-instance ((class meta2) &key)
  (format t "meta2")
  (call-next-method))

(register-m1-m2-pair 'test1-mixin 'meta1)
(register-m1-m2-pair 'test2-mixin 'meta2)

(with-metap-ensured
  (defclass test1 (test1-mixin) ())
  (defclass test2 (test2-mixin) ()))

(test meta-propagation-test
  (is (eq (class-of (find-class 'test2)) (find-class 'meta2)))
  (is (eq (class-of (find-class 'test1)) (find-class 'meta1)))
  (is (equal "meta1"
             (with-output-to-string (*standard-output*) (make-instance 'test1))))
  (is (equal "meta2meta1"
             (with-output-to-string (*standard-output*) (make-instance 'test2)))))

(test validate-superclass*
  (is (equal `(progn (defmethod c2mop:validate-superclass ((c meta2) (s meta1)) t)
                     (defmethod c2mop:validate-superclass ((c meta2) (s meta2)) t)
                     (defmethod c2mop:validate-superclass ((c meta1) (s meta2)) nil))
             (macroexpand
              '(validate-superclass* (meta2 meta1 t) (meta2 meta2 t) (meta1 meta2 nil))))))

(test illegale-specifing-case-test
  (signals simple-error
    (with-metap-ensured (defclass test2-2 (test2-mixin) () (:metaclass meta1))))
  (signals simple-error
    (with-metap-ensured (defclass test2-3 (test2-mixin) () (:metaclass meta2)))))
