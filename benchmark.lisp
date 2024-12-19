(in-package #:prf-user)
(use-package "ITER")
#+nil (defproblem list-zip
          ((list-of fixnum) (list-of (list-of fixnum)))
          (list-of (list-of fixnum))
          ((cdr ((list-of fixnum)) (list-of fixnum))
           (reverse ((list-of fixnum)) (list-of fixnum)))
        ((nil nil) nil)
        (((0) ((0))) ((0 0)))
        (((0 0) ((0 1))) ((0 0) (0 1)))
        (((0 1) ((2 3))) ((0 2) (1 3)))
        (((2 3) ((1 0) (3 1))) ((2 1 0) (3 3 1)))
        (((3 0 1) ((0) (2) (3))) ((3 0) (0 2) (1 3))))
#+nil (defproblem list-transpose
          ((list-of (list-of fixnum)))
          (list-of (list-of fixnum))
          nil
        ((nil) nil)
        ((((0))) ((0)))
        ((((0 0) (0 1))) ((0 0) (0 1)))
        ((((0 1) (2 3))) ((0 2) (1 3)))
        ((((2 3) (1 0) (3 1))) ((2 1 3) (3 0 1)))
        ((((3 0 1) (0 2 3))) ((3 0) (0 2) (1 3))))

(defun nzp (x) (not (cl:= x 0)))
(defun count-odd (x y)
  (if (oddp y) (cl:1+ x) x))
(defun inc (x) (cl:1+ x))
(defun div2 (x) (cl:floor x 2))
(defun zero (x) (declare (ignore x)) 0)
(defun sorted-insert (n list) ;; they somehow used a different variant that doesn't repeatly insert
  (cond ((or (not list) (< n (car list)))
         (cons n list))
        ((= n (car list)) list)
        (t (cons (car list) (sorted-insert n (cdr list))))))
(defun count-nonnil (l)
  (loop for x in l when x sum 1))

(defproblem nat-fib
    (fixnum) fixnum
    (n)
  (if (> n 1)
      (+ (recurse (- n 1)) (recurse (- n 2)))
      1))
(setf (get 'nat-fib :library-functions) '((+ (fixnum fixnum) fixnum))
      (get 'nat-fib :output-size) 10000)

(defproblem nat-factorial
    (fixnum) fixnum
    (n)
  (if (> n 0)
      (* n (recurse (1- n)))
      1))
(setf (get 'nat-factorial :library-functions) '((* (fixnum fixnum) fixnum)))

(defproblem nat-gcd
    (fixnum fixnum) fixnum
    (m n)
  (gcd m n))
(defun minus (x y) (min 0 (- x y)))
(setf (get 'nat-gcd :library-functions) '((minus (fixnum fixnum) fixnum)))
(defproblem nat-eq
    (fixnum fixnum) boolean
    (a b) (= a b))

(defproblem list-interperse
    (fixnum (list-of fixnum))
    (list-of fixnum) (n l)
  (if (and (consp l) (consp (cdr l)))
      (list* (car l) n (recurse n (cdr l)))
      l))

(defproblem list-subsequences
    ((list-of fixnum)) (list-of (list-of fixnum))
    (m)
  (labels ((nonempty (m)
             (if (consp m)
                 (cons (list (car m))
                       (mapcan (lambda (l) (list l (cons (car m) l)))
                               (nonempty (cdr m))))
                 nil)))
    (cons nil (nonempty m))))

(defproblem list-partition
    ((function (fixnum) boolean) (list-of fixnum)) (cons (list-of fixnum) (list-of fixnum))
    (f l)
  (cons (remove-if-not f l)
        (remove-if f l)))

#+nil (scrape-problems "benchmark2.lisp")

(defproblem bool-band
    (boolean boolean) boolean
    (a b) (and a b))

(defproblem bool-bor
    (boolean boolean) boolean
    (a b) (or a b))

(defproblem bool-impl
    (boolean boolean) boolean
    (a b) (or (not a) b))

(defproblem bool-neg
    (boolean) boolean
    (a) (not a))

(defproblem bool-xor
    (boolean boolean) boolean
    (a b) (if a (not b) b))

(defproblem nat-max
    (fixnum fixnum) fixnum
    (a b) (max a b))
(defproblem nat-pred
    (fixnum) fixnum
    (n) (if (zerop n) 0 (1- n)))
(setf (get 'nat-pred :nonrecursive) t)
(defproblem nat-iseven
    (fixnum) boolean
    (n) (evenp n))
(defproblem nat-sum
    (fixnum fixnum) fixnum
    (a b) (+ a b))
(defproblem nat-mul
    (fixnum fixnum) fixnum
    (a b) (* a b))
(setf (get 'nat-mul :library-functions) '((+ (fixnum fixnum) fixnum)))
(defproblem nat-exp
    (fixnum fixnum) fixnum
    (a b) (expt a b))
(setf (get 'nat-exp :library-functions) '((* (fixnum fixnum) fixnum))
      (get 'nat-exp :output-size) 1000)
(defproblem list-append
    ((list-of fixnum) (list-of fixnum)) (list-of fixnum)
    (a b) (append a b))
(defproblem list-compress
    ((list-of fixnum)) (list-of fixnum)
    (l)
  (if (consp l)
      (if (eq (car l) (cadr l))
          (recurse (cdr l))
          (cons (car l) (recurse (cdr l))))
      nil))
(setf (get 'list-compress :library-functions) '((= (fixnum fixnum) boolean)))
(defproblem list-concat
    ((list-of (list-of fixnum))) (list-of fixnum)
    (ll) (apply #'append ll))
(setf (get 'list-concat :library-functions)
      '((append ((list-of fixnum) (list-of fixnum)) (list-of fixnum))))
(defproblem list-drop
    (fixnum (list-of fixnum)) (list-of fixnum)
    (n l) (nthcdr n l))
(defproblem list-even-parity
    ((list-of boolean)) boolean
    (l) (evenp (length (remove-if-not #'identity l))))
(defproblem list-filter
    ((function (fixnum) boolean) (list-of fixnum)) (list-of fixnum)
    (f l) (remove-if-not f l))
(defproblem list-fold
    ((function (fixnum fixnum) fixnum) fixnum (list-of fixnum)) fixnum
    (f n l) (reduce f l :initial-value n :from-end t))
(defproblem list-hd
    ((list-of fixnum)) fixnum
    (l) (if (consp l) (car l) 0))
(setf (get 'list-hd :nonrecursive) t)
(defproblem list-inc
    ((list-of fixnum)) (list-of fixnum)
    (l) (mapcar #'1+ l))
(defproblem list-last
    ((list-of fixnum)) (list-of fixnum)
    (l) (last l))
(defproblem list-length
    ((list-of fixnum)) fixnum
    (l) (length l))
(defproblem list-map
    ((function (fixnum) fixnum) (list-of fixnum)) (list-of fixnum)
    (f l) (mapcar f l))
(defproblem list-nth
    (fixnum (list-of fixnum)) fixnum
    (n l) (or (nth n l) 0))
(defproblem list-pairwise-swap
    ((list-of fixnum)) (list-of fixnum)
    (l)
  (if (and (consp l) (consp (cdr l)))
      (list* (cadr l) (car l) (recurse (cddr l)))
      nil))
(defproblem list-rev
    ((list-of fixnum)) (list-of fixnum)
    (l) (reverse l))
(defproblem list-snoc
    (fixnum (list-of fixnum)) (list-of fixnum)
    (n l) (append l (list n)))
(defproblem list-sorted-insert
    (fixnum (list-of fixnum)) (list-of fixnum)
    (n l)
  (if (consp l)
      (cond ((> n (car l)) (cons (car l) (recurse n (cdr l))))
            ((= n (car l)) l)
            (t (cons n l)))
      (list n)))
(setf (get 'list-sorted-insert :library-functions)
      '((> (fixnum fixnum) boolean) (= (fixnum fixnum) boolean)))
(defproblem list-sort
    ((list-of fixnum)) (list-of fixnum)
    (l)
  (if (consp l)
      (sorted-insert (car l) (recurse (cdr l)))
      nil))
(setf (get 'list-sort :library-functions) '((sorted-insert (fixnum (list-of fixnum)) (list-of fixnum))))
(defproblem list-stutter
    ((list-of fixnum)) (list-of fixnum)
    (l) (mapcan (lambda (n) (list n n)) l))
(defproblem list-sum
    ((list-of fixnum)) fixnum
    (l) (reduce #'+ l))
(setf (get 'list-sum :library-functions) '((+ (fixnum fixnum) fixnum)))

(defproblem list-take
    (fixnum (list-of fixnum)) (list-of fixnum)
    (n l)
  (if (> n 0)
      (cons (or (car l) 0) (recurse (1- n) (cdr l)))
      nil))
(defproblem list-tl
    ((list-of fixnum)) (list-of fixnum)
    (l) (cdr l))
(setf (get 'list-tl :nonrecursive) t)

(defproblem tree-binsert
    ((tree-of fixnum) fixnum) (tree-of fixnum)
    (tree n)
  (if (consp tree)
      (cond ((> n (car tree))
             (list* (car tree)
                    (cadr tree)
                    (recurse (cddr tree) n)))
            ((= n (car tree))
             tree)
            (t
             (list* (car tree)
                    (recurse (cadr tree) n)
                    (cddr tree))))
      (list* n nil nil)))
(setf (get 'tree-binsert :library-functions)
      '((> (fixnum fixnum) boolean) (= (fixnum fixnum) boolean)))

(setf (get 'tree-binsert :library-functions)
      '((> (fixnum fixnum) boolean) (= (fixnum fixnum) boolean)))

(defproblem tree-collect-leaves
    ((tree-of boolean)) (list-of boolean)
    (tree)
  (cond
    ((consp tree)
     (append (recurse (cadr tree))
             (list (car tree))
             (recurse (cddr tree))))
    (t nil)))
(setf (get 'tree-collect-leaves :library-functions)
      '((append ((list-of fixnum) (list-of fixnum)) (list-of fixnum))))

(defproblem tree-1-collect-leaves
    ((tree-1-of boolean)) (list-of boolean)
    (tree)
  (cond
    ((consp tree)
     (append (recurse (cadr tree))
             (list (car tree))
             (recurse (cddr tree))))
    (t (list tree))))

(defproblem tree-count-leaves
    ((tree-of boolean)) fixnum
    (tree)
  (if (consp tree)
      (+ (recurse (cadr tree))
         (recurse (cddr tree)))
      1))
(setf (get 'tree-count-leaves :library-functions) '((+ (fixnum fixnum) fixnum)))

(defproblem tree-1-count-leaves
    ((tree-1-of boolean)) fixnum
    (tree)
  (if (consp tree)
      (+ (recurse (cadr tree))
         (recurse (cddr tree)))
      1))
(setf (get 'tree-1-count-leaves :library-functions) '((+ (fixnum fixnum) fixnum)))

(defproblem tree-count-nodes
    ((tree-of fixnum)) fixnum
    (tree)
  (if (consp tree)
      (+ 1 (recurse (cadr tree))
         (recurse (cddr tree)))
      0))
(setf (get 'tree-count-nodes :library-functions) '((+ (fixnum fixnum) fixnum)))

(defproblem tree-inorder
    ((tree-of fixnum)) (list-of fixnum)
    (tree)
  (if (consp tree)
      (append (recurse (cadr tree))
              (list (car tree))
              (recurse (cddr tree)))
      nil))
(setf (get 'tree-inorder :library-functions)
      '((append ((list-of fixnum) (list-of fixnum)) (list-of fixnum))))

(defproblem tree-1-inorder
    ((tree-1-of fixnum)) (list-of fixnum)
    (tree)
  (if (consp tree)
      (append (recurse (cadr tree))
              (list (car tree))
              (recurse (cddr tree)))
      (list tree)))
(setf (get 'tree-1-inorder :library-functions)
  '((append ((list-of fixnum) (list-of fixnum)) (list-of fixnum))))

(defproblem tree-postorder
    ((tree-of fixnum)) (list-of fixnum)
    (tree)
  (if (consp tree)
      (append (recurse (cadr tree))
              (recurse (cddr tree))
              (list (car tree)))
      nil))
(setf (get 'tree-postorder :library-functions)
      '((append ((list-of fixnum) (list-of fixnum)) (list-of fixnum))))

(defproblem tree-preorder
    ((tree-of fixnum))
    (list-of fixnum)
    (tree)
  (if (consp tree)
      (append (list (car tree))
              (recurse (cadr tree))
              (recurse (cddr tree)))
      nil))
(setf (get 'tree-preorder :library-functions)
      '((append ((list-of fixnum) (list-of fixnum)) (list-of fixnum))))

(defproblem tree-map
    ((function (fixnum) fixnum) (tree-of fixnum))
    (tree-of fixnum)
    (f tree)
  (if (consp tree)
      (list* (funcall f (car tree))
             (recurse f (cadr tree))
             (recurse f (cddr tree)))
      nil))

(defproblem tree-nodes-at-level
    ((tree-of boolean) fixnum) fixnum
    (tree n)
  (if (> n 0)
      (if (consp tree)
          (+ (recurse (cadr tree) (1- n))
             (recurse (cddr tree) (1- n)))
          0)
      1))
(setf (get 'tree-nodes-at-level :library-functions) '((+ (fixnum fixnum) fixnum)))

(setf *problems*
      '(bool-band bool-bor bool-impl bool-neg bool-xor
        nat-max nat-pred nat-iseven nat-sum nat-mul nat-exp nat-fib nat-factorial
        list-interperse #+nil list-subsequences list-partition
        list-append list-compress list-concat list-drop list-even-parity list-filter list-fold list-hd
        list-inc list-last list-length list-map list-nth list-pairwise-swap list-rev
        list-snoc list-sorted-insert list-sort list-stutter list-sum list-take list-tl
        tree-binsert tree-collect-leaves tree-count-leaves tree-count-nodes
        tree-inorder tree-postorder tree-preorder tree-map
        tree-nodes-at-level))
#+nil (setf prf::*problems*
      '(nat-max nat-pred nat-iseven nat-sum nat-mul nat-exp nat-fib nat-factorial
        list-interperse list-subsequences list-partition
        list-append list-compress list-concat list-drop list-even-parity list-filter list-fold list-hd
        list-inc list-last list-length list-map list-nth list-pairwise-swap list-rev
        list-snoc list-sorted-insert list-sort list-stutter list-sum list-take list-tl
        tree-binsert tree-collect-leaves tree-count-leaves tree-count-nodes
        tree-inorder tree-postorder tree-preorder tree-map
        tree-nodes-at-level))
(in-package #:prf)
(setq *tests* (load-table (asdf:system-relative-pathname "para" "tests.lisp"))
      *examples* (load-table (asdf:system-relative-pathname "para" "examples.lisp")))
