;;;; metap.lisp
;;;;  
;;;; Copyright 2015 hipeta (rhipeta@gmail.com)
;;;;
;;;; This software is released under the MIT License.
;;;; http://opensource.org/licenses/mit-license.php

(in-package :cl-user)
(defpackage metap
  (:use :cl)
  (:import-from :alexandria :compose)
  (:export :*metap-m1-m2-pairs*
           :register-m1-m2-pair
           :clear-m1-m2-pairs))
(in-package :metap)

(defparameter *metap-m1-m2-pairs* nil)

(defun register-m1-m2-pair (m1class m2class)
  (pushnew (cons m1class m2class) *metap-m1-m2-pairs* :test 'equal))

(defun clear-m1-m2-pairs ()
  (setf *metap-m1-m2-pairs* nil))

(defmethod c2mop:ensure-class-using-class :around (class name &rest args
                                                   &key
                                                     direct-default-initargs
                                                     direct-slots
                                                     direct-superclasses
                                                     metaclass &allow-other-keys)
  (flet ((apply-m2class (m1class m2class)
           (let ((allowed-metaclasses `(standard-class ',m2class)))
             (if (and metaclass (member metaclass allowed-metaclasses))
                 (call-next-method class name
                                   :direct-default-initargs direct-default-initargs
                                   :direct-slots direct-slots
                                   :direct-superclasses direct-superclasses
                                   :metaclass m2class)
                 (error "can not specify ~a as metaclass because this class ~a is a subclass of ~a."
                        metaclass name m1class)))))
    (let ((superclasses (mapcan (compose #'c2mop:compute-class-precedence-list #'find-class)
                                direct-superclasses)))
      (loop for (m1class . m2class) in *metap-m1-m2-pairs*
         when (member (find-class m1class) superclasses) return (apply-m2class m1class m2class)
         finally (return (call-next-method))))))

