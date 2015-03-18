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
           :validate-superclass*
           :enable-metap
           :disable-metap
           :with-metap-ensured))
(in-package :metap)

(defparameter *metap-m1-m2-pairs* nil)

(defun register-m1-m2-pair (m1class m2class)
  (-<> (cons (find-class m1class) (find-class m2class))
    (progn (when (member <> *metap-m1-m2-pairs* :test 'equal)
             (warn "~a and ~a pair is already registered." (car <>) (cdr <>)))
           <>)
    (push *metap-m1-m2-pairs*)))

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

(defconstant +ensure-class-origin+ #'c2mop:ensure-class)

(defun enable-metap ()
  #+sbcl
  (sb-ext:without-package-locks
    (fmakunbound 'c2mop:ensure-class)
    (defun c2mop:ensure-class (name &rest args)
      (symbol-macrolet ((metaclass (getf args :metaclass))
                        (direct-superclasses (getf args :direct-superclasses)))
        (flet ((apply-m2class (m1class m2class)
                 (when (and metaclass (not (eq metaclass 'standard-class)))
                   (error "Can not specify :metaclass because this class ~a is a subclass of ~a."
                          name m1class))
                 (setf metaclass (class-name m2class)))
               (find-class% (symbol-or-class)
                 (if (symbolp symbol-or-class)
                     (find-class symbol-or-class)
                     symbol-or-class)))
          (let ((precedense-list
                 (compute-precedense-list (mapcar #'find-class% direct-superclasses))))
            (loop for c in precedense-list do
                 (let ((pair (car (member c *metap-m1-m2-pairs* :key #'car))))
                   (when pair
                     (apply-m2class (car pair) (cdr pair))
                     (return))))
            (apply #'c2mop:ensure-class-using-class
                   (ignore-errors (find-class name)) name args))))))
  #-sbcl
  (progn
    (fmakunbound 'c2mop:ensure-class)
    (defun c2mop:ensure-class (name &rest args)
      (symbol-macrolet ((metaclass (getf args :metaclass))
                        (direct-superclasses (getf args :direct-superclasses)))
        (flet ((apply-m2class (m1class m2class)
                 (when (and metaclass (not (eq metaclass 'standard-class)))
                   (error "Can not specify :metaclass because this class ~a is a subclass of ~a."
                          name m1class))
                 (setf metaclass (class-name m2class)))
               (find-class% (symbol-or-class)
                 (if (symbolp symbol-or-class)
                     (find-class symbol-or-class)
                     symbol-or-class)))
          (let ((precedense-list
                 (compute-precedense-list (mapcar #'find-class% direct-superclasses))))
            (loop for c in precedense-list do
                 (let ((pair (car (member c *metap-m1-m2-pairs* :key #'car))))
                   (when pair
                     (apply-m2class (car pair) (cdr pair))
                     (return))))
            (apply #'c2mop:ensure-class-using-class
                   (ignore-errors (find-class name)) name args)))))))

(defun disable-metap (origin)
  #+sbcl
  (sb-ext:without-package-locks
    (setf (symbol-function 'c2mop:ensure-class) origin))
  #-sbcl
  (setf (symbol-function 'c2mop:ensure-class) origin))

(defmacro with-metap-ensured (&body body)
  (let ((origin (gensym)))
    `(let ((,origin #'c2mop:ensure-class))
       (unwind-protect
            (progn
              (enable-metap)
              (progn ,@body))
         (disable-metap ,origin)))))
