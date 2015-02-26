;;;; metap.lisp
;;;;  
;;;; Copyright 2015 hipeta (rhipeta@gmail.com)
;;;;
;;;; This software is released under the MIT License.
;;;; http://opensource.org/licenses/mit-license.php

(in-package :cl-user)
(defpackage metap
  (:use :cl :cl-thread-macro)
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

(defclass node ()
  ((class :initform nil :initarg :class)
   (supers :initform nil :initarg :supers)
   (subs :initform nil :initarg :subs)
   (footprint :initform nil)
   (junction-count :initform 0)))

(defun construct-lattice (superclasses)
  (let ((lattice (make-hash-table :test 'eq)))
    (labels ((add-node (node)
               (with-slots (class) node
                 (or (gethash class lattice) (setf (gethash class lattice) node))))
             (get-node (class) (gethash class lattice))
             (rec (target superclasses)
               (with-slots (class supers subs) target
                 (some->> superclasses
                   (mapcar (lambda (s)
                             (if (get-node s)
                                 (with-slots (subs junction-count) (get-node s)
                                   (setf subs (->> (nreverse subs)
                                               (adjoin target)
                                                nreverse))
                                   (setf junction-count (1- (length subs)))
                                   (get-node s))
                                 (add-node (make-instance 'node
                                                          :class s
                                                          :subs (cons target nil))))))
                   (setf supers)
                   (mapcar (lambda (s)
                             (with-slots (class) s
                               (rec s (c2mop:class-direct-superclasses class)))))))))
      (symbol-macrolet ((base (gethash 'base lattice)))
        (setf base (make-instance 'node :class 'base))
        (rec base superclasses)
        base))))

(defun display-node-data (node)
  (with-slots (class supers subs junction-count) node
    (format t "~a  junc-count:~a supers:~a subs:~a~%"
            class
            junction-count
            (mapcar (lambda (n) (ignore-errors (class-name (slot-value n 'class))))
                    supers)
            (mapcar (lambda (n) (ignore-errors (class-name (slot-value n 'class))))
                    subs))))

(defun display-lattice (node)
  (let (top)
    (labels ((rec-upward (node)
               (with-slots (supers) node
                 (display-node-data node)
                 (if (null supers) (setf top node) (mapc #'rec-upward supers)))
               top)
             (rec-downward (node)
               (with-slots (subs) node
                 (display-node-data node)
                 (if (null subs) node (mapc #'rec-downward subs)))))
      (-> node
        (-<> (progn (format t "Upwards:~%") <>)) rec-upward
        (-<> (progn (format t "~%Downwards:~%") <>)) rec-downward))))
 
(defun compute-precedense-list (direct-superclasses)
  (let ((root (construct-lattice direct-superclasses))
        (reverse-precedense-list nil))
    (labels ((rec (node)
               (with-slots (class supers footprint junction-count) node
                 (cond ((null supers) (push class reverse-precedense-list))
                       ((< 0 junction-count) (decf junction-count))
                       (footprint nil)
                       (t (setf footprint t)
                          (push class reverse-precedense-list)
                          (mapc #'rec supers))))))
      (rec root)
      (cdr (nreverse reverse-precedense-list)))))

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
           (some-<> (car (member (class-name c) *metap-m1-m2-pairs* :key #'car))
             (apply-m2class (car <>) (cdr <>))
             (return <>))
         finally (return (call-next-method))))))

