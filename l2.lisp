(defpackage #:prf/l2
  (:use #:cl #:metabang-bind #:iter #:prf)
  (:shadow #:solve-problem #:*results*)
  (:export #:*results*)
  (:local-nicknames (#:s #:scaena) (#:a #:alexandria) (#:t #:trivia)
                    (#:u #:prf-user)))
(in-package #:prf/l2)

(defun value->json (value type)
  (t:ematch type
    ((list 'list-of element-type)
     (format nil "[~{~a~^ ~}]"
             (mapcar (a:rcurry #'value->json element-type) value)))
    ((list 'tree-of element-type)
     (if (consp value)
         (format nil "{~a ~a ~a}"
                 (value->json (car value) element-type)
                 (value->json (cadr value) type)
                 (value->json (cddr value) type))
         (format nil "{}")))
    ('fixnum value)
    ('boolean (if value "true" "false"))
    ((list 'cons _ _) (error "unsupported"))
    ((list* 'function _)
     (error "unsupported"))))

(defun value->ocaml (value type)
  (t:ematch type
    ((list 'list-of element-type)
     (format nil "[~{~a~^; ~}]"
             (mapcar (a:rcurry #'value->ocaml element-type) value)))
    ((list 'tree-of element-type)
     (if (consp value)
         (format nil "(tree ~a [~a; ~a])"
                 (value->ocaml (car value) element-type)
                 (value->ocaml (cadr value) type)
                 (value->ocaml (cddr value) type))
         (format nil "{}")))
    ('fixnum value)
    ('boolean (if value "true" "false"))
    ((list 'cons _ _) (error "unsupported"))
    ((list* 'function _)
     (error "unsupported"))))

(defun l2-json (symbol examples)
  (bind (((:plist argument-types result-type) (symbol-plist symbol)))
    (format nil "{
  \"name\": \"test\",
  \"kind\": \"examples\",
  \"blacklist\" : [~{~s~^, ~}],
  \"contents\": {
    \"examples\": [~{
      \"~a\"~^, ~}
    ],
    \"background\": []
  }
}"
            (cons (string-downcase (subseq (symbol-name symbol) (1+ (position #\- (symbol-name symbol)))))
                  ;; NAT-MAX => max
                  ;; usually a good default for what we want to blacklist
                  (get symbol 'blacklist))
            (mapcar
             (lambda (ex)
               (format nil "(test~{ ~a~}) -> ~a"
                       (mapcar #'value->json (car ex) argument-types)
                       (value->json (cadr ex) result-type)))
             examples))))

(defun l2-test-ocaml (symbol program)
  (bind (((:plist argument-types result-type) (symbol-plist symbol)))
    (format nil "let test = (~a) in ~{(~a)~^ && ~}"
            program
            (mapcar (lambda (test)
                      (format nil "(test~{ ~a~})=~a"
                              (mapcar #'value->ocaml (car test) argument-types)
                              (value->ocaml (cadr test) result-type)))
                    (gethash symbol *tests*)))))

(defun value->syrup (value type &optional input-p)
  (labels ((process (value type)
             (t:ematch type
               ((list 'list-of element-type)
                (format nil (if input-p  "[~{~a~^, ~}]" "[~{~a~^; ~}]")
                        (mapcar (a:rcurry #'process element-type) value)))
               ((list 'tree-of element-type)
                (if (consp value)
                    (format nil "(Node (~a, ~a, ~a))"
                            (process (cadr value) type)
                            (process (car value) element-type)
                            (process (cddr value) type))
                    (format nil "Leaf")))
               ('fixnum value)
               ('boolean (if input-p
                             (if value "T ()" "F ()")
                             (if value "true" "false")))
               ((list 'cons ta tb) (format nil "(~a, ~a)" (process (car value) ta) (process (cdr value) tb)))
               ((list* 'function _)
                (if (and (symbolp value) (get value 'syrup))
                    (if input-p (get value 'syrup)
                        (format nil "\"~a\"" (get value 'syrup)))
                    (error "unsupported"))))))
    (process value type)))

(defun type->syrup (type)
  (t:ematch type
    ((list 'list-of element-type) (format nil "(Denotation.list ~a)" (type->syrup element-type)))
    ((list 'tree-of element-type) (format nil "(Denotation.tree ~a)" (type->syrup element-type)))
    ('fixnum "Denotation.int")
    ('boolean "Denotation.bool")
    ((list 'cons ta tb) (format nil "(Denotation.args2 ~a ~a)"
                                (type->syrup ta) (type->syrup tb)))
    ((list* 'function _)  "Denotation.var")))

(defun argument-types->syrup (types)
  (format nil "~a~{~a~^ ~}"
          (if (> (length types) 1)
              (format nil "Denotation.args~a " (length types))
              "")
          (mapcar #'type->syrup types)))

(setf (get 'u:nzp 'syrup) "isNonzero")
(setf (get 'evenp 'syrup) "isEven")
(setf (get 'u:inc 'syrup) "inc")
(setf (get 'u:div2 'syrup) "div2")
(setf (get '+ 'syrup) "add")
(setf (get 'u:count-odd 'syrup) "countOdd")

(defun syrup-reference (symbol)
  (bind (((:plist argument-types result-type) (symbol-plist symbol))
         (*print-case* :downcase))
    (format nil "
( \"~a\",
      proj
        {
          function_name = \"~a\";
          k_max = 20;
          d_in = ~a;
          d_out = ~a;
          input = (fun ?(exclude_base = false) _ -> failwith \"Unimplemented.\");
          assertion = [~{~a;
~}];
          func = (fun _ -> failwith \"Unimplemented.\");
          background = [];
        } );"
            symbol symbol
            (argument-types->syrup argument-types)
            (type->syrup result-type)
            (mapcar (lambda (test)
                      (format nil "((~{~a~^, ~}),~a)"
                              (mapcar #'value->syrup (car test) argument-types)
                              (value->syrup (cadr test) result-type)))
                    (gethash symbol *tests*)))))

(defun syrup-solve (symbol examples &rest options &key silent)
  (bind (((:plist argument-types result-type) (symbol-plist symbol)))
    (bind (((:values output error retval)
            (handler-bind
                ((uiop:subprocess-error
                   (lambda (c)
                     (declare (ignore c))
                     (invoke-restart 'continue))))
              (uiop:run-program (list "docker" "exec" "syrup" "timeout" "480"
                                      "/home/opam/syrup/syrup" "syrup"
                                      (string-downcase (symbol-name symbol))
                                      (format nil "~{~a~^; ~}"
                                              (mapcar
                                               (lambda (ex)
                                                 (format nil "~a -> ~a"
                                                         (if (cdr argument-types)
                                                             (format nil "(~{~a~^, ~})"
                                                                     (mapcar (a:rcurry #'value->syrup t)
                                                                             (car ex) argument-types))
                                                             (value->syrup (caar ex) (car argument-types) t))
                                                         (value->syrup (cadr ex) result-type t)))
                                               examples)))

                                :output :string
                                :error-output :string)))
           ((time correct-p)
            (cond ((zerop retval)
                   (bind (((time correct)
                           (split-sequence:split-sequence
                            #\Space
                            (a:lastcar (split-sequence:split-sequence #\Newline output :remove-empty-subseqs t)))))
                     (write-string output)
                     (list time (equalp correct "true"))))
                  ((> retval 127) ; we probably manually killed the process
                   (list sb-ext:single-float-positive-infinity nil))
                  (t (list sb-ext:single-float-positive-infinity (string-trim '(#\Newline) error))))))
      (format *trace-output* "~a result: ~a ~a~%" symbol time correct-p)
      (if silent
          (list time correct-p examples)
          (push (list options time correct-p examples)
                (gethash symbol *results*))))))

(setf (get '> 'ml) "builtin >")
(setf (get '= 'ml) "builtin =")
(setf (get 'u::sorted-insert 'ml)
      "let rec insert =
fun xs n ->
  if xs = [] then (n :: []) else
  if n < (car xs) then (n :: xs) else
  if n = (car xs) then xs else
    (car xs) :: (insert (cdr xs) n)")
(setf (get '+ 'ml) "builtin +")
(setf (get '* 'ml) "builtin *")
(setf (get 'append 'ml) "let rec append = fun l1 l2 ->
  if l1 = [] then l2 else
  if l2 = [] then l1 else
    (car l1) :: (append (cdr l1) l2)")

(defvar *results* (make-hash-table :synchronized t))
(defun solve-problem (symbol examples &rest options &key library silent)
  (with-temporary-file (ex (l2-json symbol examples))
    (with-temporary-file
        (stdlib
         (apply #'serapeum:concat
                (uiop:read-file-string
                 (ecase library
                   ((:para :para+) "/home/qh/playground/L2/components/para.ml")
                   ((:all) "/home/qh/playground/L2/components/stdlib-para.ml")
                   ((nil t) "/home/qh/playground/L2/components/stdlib.ml")))
                (when (eq library t)
                  (unless (set-difference (mapcar #'car (get symbol :library-functions))
                                          '(+ * = > append))
                    (return-from solve-problem)))
                (when (member library '(:para+ t :all))
                  (unless (get symbol :library-functions)
                    (return-from solve-problem))
                  (mapcar (lambda (f) (serapeum:concat "
" (get (car f) 'ml)))
                          (get symbol :library-functions)))))
      (bind (time
             ((:values output error retval)
              (sb-ext:call-with-timing
               (lambda (&key real-time-ms &allow-other-keys)
                 (setq time (/ real-time-ms 1000.0)))
               (lambda ()
                 (handler-bind ((uiop:subprocess-error
                                  (lambda (c)
                                    (declare (ignore c))
                                    (invoke-restart 'continue))))
                   (uiop:run-program (list "timeout" "480" "l2" "synth" "-l" stdlib
                                           (namestring ex) "-dd" "higher_order,input_ctx" "-q")
                                     :output :string
                                     :error-output :string)))))
             (correct-p
              (if (= retval 0)
                  (string-equal
                   (with-temporary-file (te (l2-test-ocaml symbol output))
                     (uiop:run-program (list "timeout" "1" "l2" "eval" "--library" stdlib
                                             (namestring te))
                                       :output :string
                                       :error-output :string))
                   "true")
                  error)))
        (if silent
            (list time correct-p examples output)
            (push (list options time correct-p examples output)
               (gethash symbol *results*)))))))

(setf (get 'u:list-interperse 'blacklist) '("intersperse")
      (get 'u:list-length 'blacklist) '("len")
      (get 'u:list-rev 'blacklist) '("reverse")
      (get 'u:list-sort 'blacklist) '("dedup")
      (get 'u:nat-exp 'blacklist) '("pow")
      (get 'u:nat-factorial 'blacklist) '("fact")
      (get 'u:nat-iseven 'blacklist) '("even")
      (get 'u:nat-mul 'blacklist) '("*")
      (get 'u:nat-sum 'blacklist) '("+"))

(defun l2-support-p (symbol)
  (labels ((process (type)
             (if (consp type)
                 (and (process (car type)) (process (cdr type)))
                 (not (or (eq type 'function)
                          (eq type 'cons))))))
    (process (cons (get symbol :argument-types)
                   (get symbol :result-type)))))
