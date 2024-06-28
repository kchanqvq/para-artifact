(in-package "PRF")

(defun generate-example-set ()
  (iter (for problem in *problems*)
    (setf (gethash problem *tests*)
          (generate-examples problem 40)))

  (iter (for problem in *problems*)
    (setf (gethash problem *examples*)
          (iter (for i below 10)
            (collect (generate-examples problem 20)))))

  (save-table *examples* "examples.lisp")
  (save-table *tests* "tests.lisp"))

(defvar *tasks*
  (iter outer (for p in *problems*)
    (iter (for e in (gethash p *examples*))
      (in outer
          (unless (member problem '(u:list-sort u:list-sorted-insert u:list-compress u:tree-binsert))
            (collect (list problem examples)))
          (when (get problem :library-functions)
            (collect (list problem examples :library t)))))))

(in-package "PRF/SMYTH")

(defun generate-data ()
  (let ((tasks prf::*tasks*)
        (*results* (load-table "smyth-results.lisp")))
    (lparallel.kernel-util:with-temp-kernel (8)
      (lparallel:pmapc
       #'solve-problem
       :parts (length tasks)
       tasks))
    (save-table *results* "smyth-results.lisp")))

(in-package "PRF/L2")

(defun generate-data ()
  (let ((tasks (remove-if-not (lambda (r) (l2-support-p (car r)))
                              prf::*tasks*))
        (*results* (load-table "l2-results.lisp")))
    (lparallel.kernel-util:with-temp-kernel (4)
      (lparallel:pmapc
       #'solve-problem
       :parts (length tasks)
       tasks))
    (save-table *results* "l2-results.lisp")))

(in-package "PRF")

(defun generate-data ()
  (let ((tasks prf::*tasks*)
        (*results* (load-table "prf-results.lisp")))
    (iter (for task in tasks)
      (iter (for depth in '(3 2 1))
        (apply #'solve-problem (append task (list :depth depth)))))
    (save-table *results* "prf-results.lisp")))

(defun generate-data-bfs ()
  (let ((tasks prf::*tasks*)
        (*results* (load-table "prf-results-bfs.lisp")))
    (iter (for task in tasks)
      (iter (for depth in '(3 2 1))
        (apply #'solve-problem (append task (list :depth depth :method 'bfs)))))
    (save-table *results* "prf-results-bfs.lisp")))

(defun generate-data-serial ()
  (let ((tasks prf::*tasks*)
        (*results* (load-table "prf-results-serial.lisp")))
    (lparallel:pmapc
     (lambda (task)
       (iter (for depth in '(3 2 1))
         (apply #'solve-problem (append task (list :depth depth
                                                   :method 'serial-search
                                                   :total-timeout 480)))))
     :parts (length tasks)
     tasks)
    (save-table *results* "prf-results-serial.lisp")))

(defun report ()
  (bind ((stream t)
         (prf-results (load-table "prf-results.lisp"))
         (prf-results-serial (load-table "prf-results-serial.lisp"))
         (bfs-results (load-table "prf-results-bfs.lisp"))
         (smyth-results (load-table "smyth-results.lisp"))
         (l2-results (load-table "l2-results-with.lisp"))
         ((:values problems-1 problems-2) (serapeum:partition #'prf/l2::l2-support-p *problems*)))
    (iter (for library in '(nil t))
      (iter (for problem in problems-1)
        (when (if library
                  (get problem :library-functions)
                  (not (member problem '(u:list-sort u:list-sorted-insert u:list-compress u:tree-binsert))))
          (format stream "~a ~a ~{& ~a & ~,3f & ~a~} ~{& ~a & ~,3f & ~a~} ~{& ~a & ~,3f & ~a~} ~{& ~,3f & ~a~} ~{& ~,3f & ~a~} ~{& ~,3f & ~a~}\\\\~%"
                  (string-downcase (symbol-name problem))
                  (if library
                      (format nil "(~{~a~^,~})"
                              (mapcar (lambda (row)
                                        (let ((function (car row)))
                                          (or (get function :latex)
                                              (string-downcase (symbol-name function)))))
                                      (get problem :library-functions)))
                      "")
                  (prf-results problem prf-results library)
                  (prf-results problem bfs-results library)
                  (prf-results problem prf-results-serial library 500)
                  (analyze-results (find-result problem smyth-results library 3))
                  (if library
                      (analyze-results
                       (or (find-result problem l2-results t 3)
                           (find-result problem l2-results nil 3)))
                      (analyze-results (find-result problem l2-results nil 3)))
                  (if library
                      (analyze-results (find-result problem l2-results :para+ 3))
                      (analyze-results (find-result problem l2-results :para 3)))))))
    (terpri)
    (iter (for library in '(nil t))
      (iter (for problem in problems-2)
        (when (if library
                  (get problem :library-functions)
                  (not (member problem '(u:list-sort u:list-sorted-insert u:list-compress u:tree-binsert))))
          (format stream "~a ~a ~{& ~a & ~,3f & ~a~} ~{& ~a & ~,3f & ~a~} ~{& ~a & ~,3f & ~a~} ~{& ~,3f & ~a~}\\\\~%"
                  (string-downcase (symbol-name problem))
                  (if library
                      (format nil "(~{~a~^,~})"
                              (mapcar (lambda (row)
                                        (let ((function (car row)))
                                          (or (get function :latex)
                                              (string-downcase (symbol-name function)))))
                                      (get problem :library-functions)))
                      "")
                  (prf-results problem prf-results library)
                  (prf-results problem bfs-results library)
                  (prf-results problem prf-results-serial library 600)
                  (analyze-results (find-result problem smyth-results library 3))))))))

(defun report-bar ()
  (bind ((stream t)
         (prf-results (load-table "prf-results.lisp"))
         (prf-results-serial (load-table "prf-results-serial.lisp"))
         (bfs-results (load-table "prf-results-bfs.lisp"))
         (smyth-results (load-table "smyth-results.lisp"))
         (l2-results (load-table "l2-results-with.lisp"))
         ((:values problems-1 problems-2) (serapeum:partition #'prf/l2::l2-support-p *problems*))
         (total-row (list 0 0 0))
         (bfs-row (list 0 0 0))
         (serial-row (list 0 0 0))
         (smyth-row (list 0 0 0))
         (l2-stdlib-row (list 0 0 0))
         (l2-para-row (list 0 0 0)))
    (iter (for library in '(nil t))
      (iter (for problem in problems-1)
        (when (if library
                  (get problem :library-functions)
                  (not (member problem '(u:list-sort u:list-sorted-insert u:list-compress u:tree-binsert))))
          (bind (((depth avg n) (prf-results problem prf-results library)))
            #+nil (incf (nth (1- depth) total-row))
            (when (numberp n)
              (incf (nth (1- depth) total-row))
              (when (numberp (cadr (prf-results problem bfs-results library)))
                (incf (nth (1- depth) bfs-row)))
              (when (numberp (cadr (prf-results problem prf-results-serial library 500)))
                (incf (nth (1- depth) serial-row)))
              (when (numberp (car (analyze-results (find-result problem smyth-results library 3))))
                (incf (nth (1- depth) smyth-row)))
              (when (numberp (car (if library
                                      (analyze-results
                                       (or (find-result problem l2-results t 3)
                                           (find-result problem l2-results nil 3)))
                                      (analyze-results (find-result problem l2-results nil 3)))))
                (incf (nth (1- depth) l2-stdlib-row)))
              (when (numberp (car (if library
                                      (analyze-results (find-result problem l2-results :para+ 3))
                                      (analyze-results (find-result problem l2-results :para 3)))))
                (incf (nth (1- depth) l2-para-row))))))))
    (iter (for i below 3)
      (format stream "\\addplot coordinates {(1,~a/~a) (2,~a/~a) (3,~a/~a) (4,~a/~a) (5,~a/~a) (6,~a/~a)};~%"
              (nth i total-row) (nth i total-row)
              (nth i bfs-row) (nth i total-row)
              (nth i serial-row) (nth i total-row)
              (nth i smyth-row) (nth i total-row)
              (nth i l2-stdlib-row) (nth i total-row)
              (nth i l2-para-row) (nth i total-row)))
    (values)))
