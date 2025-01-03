(defpackage #:min-max-heap
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria))
  (:export #:heap #:make-heap #:copy-heap
           #:insert #:pop-min #:pop-max #:peek-min #:peak-max
           #:size #:mapheap))
(in-package #:min-max-heap)

(declaim (optimize (speed 3)))
(deftype key-type () 'single-float)
(deftype value-type () 't)
(defconstant positive-infinity sb-ext:single-float-positive-infinity)
(defconstant negative-infinity sb-ext:single-float-negative-infinity)
(defconstant unbound-value 0)

(defmacro min-layer-p (index) `(oddp (integer-length (1+ ,index))))
(defmacro unbound-key (index) `(if (min-layer-p ,index) positive-infinity negative-infinity))
(defmacro parent-index (index) `(ash (1- ,index) -1))
(defmacro grandparent-index (index) `(ash (- ,index 3) -2))
(defmacro child-index (index) `(1+ (ash ,index 1)))
(defmacro grandchild-index (index) `(+ 3 (ash ,index 2)))
(defmacro op-1 (op a b)
  `(multiple-value-bind (a a-1) ,a
     (multiple-value-bind (b b-1) ,b
       (if (,op a b) (values a a-1) (values b b-1)))))
(defmacro aref-1 (array index)
  `(values (aref ,array ,index) ,index))
(defmacro ref-2 (index)
  `(values (aref key-vector ,index) (aref value-vector ,index)))
(defmacro set-2 (index values)
  `(multiple-value-bind (key-1 value-1) ,values
     (psetf (aref key-vector ,index) key-1 (aref value-vector ,index) value-1)))

(defun initialize-key-vector (array)
  (declare (type (simple-array key-type) array))
  (loop for i of-type a:array-index below (length array)
        do (setf (aref array i) (unbound-key i)))
  array)

(declaim (inline make-heap heap-value-vector heap-key-vector heap-count heap-size))
(defstruct (heap (:constructor make-heap (&optional (size 255))))
  (value-vector (make-array size :element-type 'value-type) :type (simple-array value-type))
  (key-vector (initialize-key-vector (make-array size :element-type 'key-type))
   :type (simple-array key-type))
  (count 0 :type a:array-length))

(defmacro push-down (op)
  `(progn
     (set-2 index (values key value))
     (loop
       (flet ((fix-parent (&aux (parent-index (parent-index index)))
                (when (< (aref key-vector parent-index) key)
                  (rotatef (aref key-vector parent-index) key)
                  (rotatef (aref value-vector parent-index) value))))
         (declare (inline fix-parent) (optimize (safety 0)))
         (let ((grandchild-index (grandchild-index index))
               (child-index (child-index index)))
           (cond ((< (+ grandchild-index 2) last) ; Each children have at least 1 child.
                  (multiple-value-bind (extreme-key extreme-index)
                      (op-1 ,op (op-1 ,op (aref-1 key-vector grandchild-index)
                                      (aref-1 key-vector (+ 1 grandchild-index)))
                            (op-1 ,op (aref-1 key-vector (+ 2 grandchild-index))
                                  (aref-1 key-vector (+ 3 grandchild-index))))
                    (if (,op extreme-key key)
                        (progn
                          (set-2 index (ref-2 extreme-index))
                          (setf index extreme-index)
                          (fix-parent))
                        (return))))
                 ((< grandchild-index last) ; Left child has at least 1 child
                  (multiple-value-bind (extreme-key extreme-index)
                      (op-1 ,op (aref-1 key-vector grandchild-index)
                            (aref-1 key-vector (1+ grandchild-index)))
                    (if (,op extreme-key (aref key-vector (1+ (child-index index))))
                        (progn
                          (when (,op extreme-key key)
                            (set-2 index (ref-2 extreme-index))
                            (setf index extreme-index)
                            (fix-parent))
                          (return))
                        (progn
                          (when (,op (aref key-vector (1+ child-index)) key)
                            (set-2 index (ref-2 (1+ child-index)))
                            (setf index (1+ child-index)))
                          (return)))))
                 ((< (1+ child-index) last)
                  (multiple-value-bind (extreme-key extreme-index)
                      (op-1 ,op (aref-1 key-vector child-index)
                            (aref-1 key-vector (1+ child-index)))
                    (when (,op extreme-key key)
                      (set-2 index (ref-2 extreme-index))
                      (setf index extreme-index))
                    (return)))
                 ((< child-index last)
                  (when (,op (aref key-vector child-index) key)
                    (set-2 index (ref-2 child-index))
                    (setf index child-index))
                  (return))
                 (t (return))))))
     (set-2 index (values key value))))

(declaim (notinline insert pop-min))
(declaim (ftype (function (heap key-type value-type) (values)) insert))
(defun insert (heap key value)
  (declare (type heap heap) (type key-type key) (type value-type value))
  (let* ((value-vector (heap-value-vector heap))
         (key-vector (heap-key-vector heap))
         (index (heap-count heap)))
    (declare (type a:array-index index)
             (optimize (safety 0)))
    (if (< index (length key-vector))
        (progn
          (set-2 index (values key value))
          (incf (heap-count heap))
          (labels ((initial-exchange (&aux (parent-index (parent-index index)))
                     (set-2 index (ref-2 parent-index))
                     (setf index parent-index))
                   (push-up-max (&aux (grandparent-index (grandparent-index index)))
                     (loop while
                           (and (> index 2)
                                (< (aref key-vector grandparent-index) key))
                           do (set-2 index (ref-2 grandparent-index))
                           do (psetf index grandparent-index
                                     grandparent-index (grandparent-index index))))
                   (push-up-min (&aux (grandparent-index (grandparent-index index)))
                     (loop while
                           (and (> index 0)
                                (> (aref key-vector grandparent-index) key))
                           do (set-2 index (ref-2 grandparent-index))
                           do (psetf index grandparent-index
                                     grandparent-index (grandparent-index index)))))
            (declare (inline initial-exchange push-up-max push-up-min))
            (if (min-layer-p index)
                (cond
                  ((= index 0))
                  ((< (aref key-vector (parent-index index)) key)
                   (initial-exchange) (push-up-max))
                  (t (push-up-min)))
                (cond
                  ((> (aref key-vector (parent-index index)) key)
                   (initial-exchange) (push-up-min))
                  (t (push-up-max)))))
          (set-2 index (values key value)))
        (let ((last (length key-vector)))
          (when (< key (aref key-vector 0))
            (rotatef (aref key-vector 0) key)
            (rotatef (aref value-vector 0) value))
          (multiple-value-bind (extreme-key extreme-index)
              (op-1 > (aref-1 key-vector 1) (aref-1 key-vector 2))
            (unless (< extreme-key key)
              (setq index extreme-index)
              (push-down >)))))
    (values)))

(declaim (ftype (function (heap) (values value-type key-type)) pop-min))
(defun pop-min (heap)
  "Pop the minimal element in HEAP.
Return (values VALUE KEY)."
  (declare (type heap heap))
  (let ((value-vector (heap-value-vector heap))
        (key-vector (heap-key-vector heap)))
    (if (> (heap-count heap) 0)
        (multiple-value-prog1
            (values (aref (heap-value-vector heap) 0) (aref (heap-key-vector heap) 0))
          (let ((last (decf (heap-count heap))))
            (multiple-value-bind (key value) (ref-2 last)
              (set-2 last (values (unbound-key last) unbound-value))
              (unless (= last 0)
                (let ((index 0))
                  (declare (type a:array-index last index)
                           (optimize (safety 0)))
                  (push-down <))))))
        (values unbound-value 0.0))))

(defun mapheap (function heap)
  (dotimes (i (heap-count heap))
    (funcall function (aref (heap-key-vector heap) i)
             (aref (heap-value-vector heap) i))))
