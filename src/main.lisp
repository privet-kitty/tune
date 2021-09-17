(defpackage :tune
  (:use :cl)
  (:export #:deftuned #:symbol-params #:get-tuned-obj))
(in-package :tune)

(defparameter *parameters* (make-hash-table :test #'eql))
(deftype distribution ()
  '(member :uniform :loguniform :discrete-uniform :int :categorical))

(defun parse-suggest-int (params)
  (destructuring-bind (low high &optional (step 1) log) params
    (check-type low integer)
    (check-type high integer)
    (check-type step (integer 1))
    `(("low" . ,low) ("high" . ,high) ("step" . ,step)
      ("log" . ,(if log :true :false)))))

(defun parse-suggest-uniform (params)
  (destructuring-bind (low high) params
    (check-type low real)
    (check-type high real)
    `(("low" . ,(coerce low 'float)) ("high" . ,(coerce high 'float)))))

(defun parse-suggest-discrete-uniform (params)
  (destructuring-bind (low high q) params
    (check-type low real)
    (check-type high real)
    (check-type q real)
    `(("low" . ,(coerce low 'float))
      ("high" . ,(coerce high 'float))
      ("q" . ,(coerce q 'float)))))

(defun parse-categorical (params)
  (destructuring-bind (choices) params
    (check-type choices sequence)
    `(("choices" . ,(coerce choices 'list)))))

(defun parse-params (distribution params)
  (ecase distribution
    (:int
     (parse-suggest-int params))
    ((:uniform :loguniform)
     (parse-suggest-uniform params))
    (:discrete-uniform
     (parse-suggest-discrete-uniform params))
    (:categorical
     (parse-categorical params))))

(defmacro deftuned (var val distribution &rest params)
  (check-type distribution distribution)
  (parse-params distribution params) ; for validation
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (gethash ',var *parameters*)
             (list ,distribution ,@params)))
     (defparameter ,var ,val)))

(defun symbol-params (symbol)
  (declare (symbol symbol))
  (nth-value 0 (gethash symbol *parameters*)))

(defun symbol-name* (symbol)
  (let ((res (symbol-name symbol)))
    (string-downcase (if (char= #\* (aref res 0) (aref res (- (length res) 1)))
                         (subseq res 1 (- (length res) 1))
                         res))))

(declaim (ftype (function * (values list &optional)) get-tuned-obj))
(defun get-tuned-obj ()
  (loop for symbol being each hash-key of *parameters*
        using (hash-value value)
        for distribution = (car value)
        for params = (cdr value)
        collect `(:obj
                  ("name" . ,(symbol-name* symbol))
                  ("distribution" . ,(string-downcase distribution))
                  ("initial-value" . ,(symbol-value symbol))
                  ,@(parse-params distribution params))))
