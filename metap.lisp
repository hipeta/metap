;;;; metap.lisp
;;;;  
;;;; Copyright 2015 hipeta (rhipeta@gmail.com)
;;;;
;;;; This software is released under the MIT License.
;;;; http://opensource.org/licenses/mit-license.php

(in-package :cl-user)
(defpackage metap
  (:use :cl :arrow-macros)
  (:import-from :alexandria :compose)
  (:export :*metap-m1-m2-pairs*
           :register-m1-m2-pair
           :clear-m1-m2-pairs
           :validate-superclass*))
(in-package :metap)

(defparameter *metap-m1-m2-pairs* nil)

(defun register-m1-m2-pair (m1class m2class)
  (-<> (cons (find-class m1class) (find-class m2class))
    (progn (when (member <> *metap-m1-m2-pairs* :test 'equal)
             (warn "~a and ~a pair is already registered." (car <>) (cdr <>)))
           <>)
    (push *metap-m1-m2-pairs*)))

(defun clear-m1-m2-pairs ()
  (setf *metap-m1-m2-pairs* nil))

(defmacro validate-superclass* (&body validations)
  `(progn
     ,@(loop for validation in validations collect
            (destructuring-bind (meta1 meta2 validate-p) validation
              `(defmethod c2mop:validate-superclass
                   ((,(intern "C") ,meta1) (,(intern "S") ,meta2)) ,validate-p)))))

(defun compute-precedense-list (direct-superclasses)
  (let ((junction-count-table (make-hash-table))
        (reverse-precedense-list nil))
    (labels ((rec (class &optional first-p)
               (symbol-macrolet ((junction-count (gethash class junction-count-table)))
                 (let ((supers (c2mop:class-direct-superclasses class))
                       (subs (c2mop:class-direct-subclasses class)))
                   (if first-p
                       (cond ((null class) nil)
                             (junction-count (incf junction-count))
                             (t (setf junction-count 1)
                                (mapc (lambda (c) (rec c t)) supers)))
                       (cond ((null class) nil)
                             ((< 1 junction-count) (decf junction-count))
                             ((= 0 junction-count) nil)
                             (t (decf junction-count)
                                (push class reverse-precedense-list)
                                (mapc #'rec supers))))))))
      (mapc (lambda (c) (rec c t)) direct-superclasses)
      (mapc #'rec direct-superclasses)
      (nreverse reverse-precedense-list))))

(defmethod c2mop:ensure-class-using-class :around (class name &rest args
                                                   &key
                                                     direct-default-initargs
                                                     direct-slots
                                                     direct-superclasses
                                                     metaclass &allow-other-keys)
  (declare (ignorable args))
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
    (let ((precedense-list (compute-precedense-list (mapcar #'find-class direct-superclasses))))
      (loop for c in precedense-list do
           (some-<> (car (member c *metap-m1-m2-pairs* :key #'car))
             (apply-m2class (car <>) (cdr <>))
             (return))
         finally (return (call-next-method))))))
