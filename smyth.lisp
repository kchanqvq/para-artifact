(defpackage #:prf/smyth
  (:use #:cl #:metabang-bind #:iter #:prf)
  (:shadow #:solve-problem #:*results*)
  (:export #:*results*)
  (:local-nicknames (#:s #:scaena) (#:a #:alexandria) (#:t #:trivia)
                    (#:u #:prf-user)))
(in-package #:prf/smyth)
(defvar *preamble*
  "type Nat
  = Z ()
  | S Nat

type NatList
  = NatNil ()
  | NatCons (Nat, NatList)

type NatListList
  = NatListNil ()
  | NatListCons (NatList, NatListList)

type NatTree
  = NatLeaf ()
  | NatNode (NatTree, Nat, NatTree)

type Boolean
  = F ()
  | T ()

type BooleanList
  = BooleanNil ()
  | BooleanCons (Boolean, BooleanList)

type BooleanTree
  = BooleanLeaf ()
  | BooleanNode (BooleanTree, Boolean, BooleanTree)

type NatListNatList
  = NatListNatListPair (NatList, NatList)
")
(setf (get '> 'elm)
      "greater : Nat -> Nat -> Boolean
greater n1 n2 =
  case n1 of
    Z _ ->
      case n2 of
        Z _ -> F
        S _ -> F
    S m1 ->
      case n2 of
        Z _  -> T
        S m2 -> greater m1 m2")
(setf (get '= 'elm)
      "equal : Nat -> Nat -> Boolean
equal n1 n2 =
  case n1 of
    Z _ ->
      case n2 of
        Z _ -> T
        S _ -> F
    S m1 ->
      case n2 of
        Z _  -> F
        S m2 -> equal m1 m2")
(setf (get 'u::sorted-insert 'elm)
      "type Cmp
  = LT ()
  | EQ ()
  | GT ()

compare : Nat -> Nat -> Cmp
compare n1 n2 =
  case n1 of
    Z _ ->
      case n2 of
        Z _ -> EQ ()
        S _ -> LT ()
    S m1 ->
      case n2 of
        Z _  -> GT ()
        S m2 -> compare m1 m2

insert : NatList -> Nat -> NatList
insert xs n =
  case xs of
    NatNil _ -> NatCons (n, NatNil ())
    NatCons p ->
      case compare n (#2.1 p) of
        LT _ -> NatCons (n, NatCons (#2.1 p, #2.2 p))
        EQ _ -> xs
        GT _ -> NatCons (#2.1 p, insert (#2.2 p) n)")
(setf (get 'u::sorted-insert 'name) "insert")
(setf (get 'u::inc 'elm)
      "inc : Nat -> Nat
inc n = S n")
(setf (get 'u::zero 'elm) "zero : Nat -> Nat
zero n = Z ()")
(setf (get 'u::div2 'elm) "div2 : Nat -> Nat
div2 n =
  case n of
    Z _ -> Z ()
    S m1 ->
      case m1 of
        Z _ -> Z ()
        S m2 -> S (div2 m2)")
(setf (get 'u::evenp 'elm) "evenp : Nat -> Boolean
evenp n =
  case n of
    Z _  -> T ()
    S m1 ->
      case m1 of
        Z _  -> F ()
        S m2 -> evenp m2")
(setf (get 'u::nzp 'elm) "nzp : Nat -> Boolean
nzp n =
  case n of
    Z _ -> F ()
    S _ -> T ()")
(setf (get '+ 'elm) "sum : Nat -> Nat -> Nat
sum n1 n2 =
  case n1 of
    Z _ -> n2
    S m -> S (sum m n2)")
(setf (get '+ 'name) "sum")
(setf (get '* 'elm) "sum : Nat -> Nat -> Nat
sum n1 n2 =
  case n1 of
    Z _ -> n2
    S m -> S (sum m n2)
mul : Nat -> Nat -> Nat
mul n1 n2 =
  case n1 of
    Z _ -> Z
    S m -> (sum (mul m n2) n2)")
(setf (get '* 'name) "mul")

(setf (get 'u::count-odd 'elm) "isOdd : Nat -> Boolean
isOdd n =
  case n of
    Z _  -> F ()
    S m1 ->
      case m1 of
        Z _  -> T ()
        S m2 -> isOdd m2

countOdd : Nat -> Nat -> Nat
countOdd n1 n2 =
  case isOdd n2 of
    T _ -> S n1
    F _ -> n1")
(setf (get 'u::count-odd 'name) "countOdd")
(setf (get 'append 'elm) "append : NatList -> NatList -> NatList
append l1 l2 =
  case l1 of
    NatNil _ ->
      l2
    NatCons p ->
      NatCons (#2.1 p, append (#2.2 p) l2)")
(defun type->elm (type)
  (t:ematch type
    ((list 'function argument-types result-type)
     (format nil "(~{~a -> ~}~a)"
             (mapcar #'type->elm argument-types)
             (type->elm result-type)))
    ('fixnum "Nat")
    ('boolean "Boolean")
    ((list 'list-of element-type) (format nil "~a~a" (type->elm element-type) "List"))
    ((list 'tree-of element-type) (format nil "~a~a" (type->elm element-type) "Tree"))
    ((list 'cons a b) (format nil "~a~a" (type->elm a) (type->elm b)))))
(defvar *library-function-registry*)
(defun value->elm (value type)
  (t:ematch type
    ((list 'list-of element-type)
     (if (consp value)
         (format nil "~aCons (~a, ~a)"
                 (type->elm element-type)
                 (value->elm (car value) element-type)
                 (value->elm (cdr value) type))
         (format nil "~aNil" (type->elm element-type))))
    ((list 'tree-of element-type)
     (if (consp value)
         (format nil "~aNode (~a, ~a, ~a)"
                 (type->elm element-type)
                 (value->elm (cadr value) type)
                 (value->elm (car value) element-type)
                 (value->elm (cddr value) type))
         (format nil "~aLeaf" (type->elm element-type))))
    ('fixnum value)
    ('boolean (if value "T" "F"))
    ((list 'cons a b) (format nil "~aPair (~a, ~a)" (type->elm type)
                              (value->elm (car value) a) (value->elm (cdr value) b)))
    ((list* 'function _)
     (assert (symbolp value))
     (pushnew value *library-function-registry*)
     (or (get value 'name) (string-downcase (symbol-name value))))))
(defvar *results* (make-hash-table :synchronized t))
(defun examples->elm (examples argument-types result-type)
  (format nil "specifyFunction~a problem [
~{~a~^,~%~}]"
          (if (> (length argument-types) 1)
              (length argument-types)
              "")
          (mapcar
           (lambda (ex)
             (format nil "(~{~a, ~}~a)"
                     (mapcar #'value->elm (car ex) argument-types)
                     (value->elm (cadr ex) result-type)))
           examples)))
 (defun sketch-elm (symbol)
   (bind (((:plist argument-types result-type) (symbol-plist symbol)))
         (case symbol
           ((u:list-filter u:list-fold u:list-map u:tree-map)
            (format nil "problem : ~a
problem ~{~a ~} =
  let
    fix : ~a
    fix ~a = ??
  in
    fix"
                    (type->elm (list 'function argument-types result-type))
                    (mapcar #'s:gensym-like (butlast argument-types))
                    (type->elm (list 'function (last argument-types) result-type))
                    (s:gensym-like (car (last argument-types)))))
           (t (format nil "problem : ~a
problem ~{~a ~}= ??"
                      (type->elm (list 'function argument-types result-type))
                      (mapcar #'s:gensym-like argument-types))))))
(defun solve-problem (symbol examples &rest options &key library silent &allow-other-keys)
  (bind (((:plist argument-types result-type library-functions) (symbol-plist symbol))
         (library-functions (when library
                              (unless library-functions (return-from solve-problem))
                              library-functions))
         (tests (gethash symbol *tests*))
         (*library-function-registry* (mapcar #'car library-functions))
         (*print-case* :downcase)
         (*gensym-counter* 0))
    (with-temporary-file (ex (examples->elm examples argument-types result-type))
      (with-temporary-file (te (examples->elm tests argument-types result-type))
        (with-temporary-file (sketch
                              (format nil "~a~{~a~%~}~a"
                                      *preamble*
                                      (mapcar (lambda (symbol)
                                                (or (get symbol 'elm)
                                                    (error "Library ELM definition not available")))
                                              *library-function-registry*)
                                      (sketch-elm symbol)))
          (bind (((:values output error retval)
                  (handler-bind
                      ((uiop:subprocess-error
                         (lambda (c)
                           (declare (ignore c))
                           (invoke-restart 'continue))))
                    (uiop:run-program (list "smyth" "test" te sketch ex "--timeout=480")
                                      :output :string
                                      :error-output :string)))
                 ((time correct-p)
                  (cond ((zerop retval)
                         (bind (((time _ _ top-correct recursive-top-correct)
                                 (uiop:split-string (print (string-trim '(#\Newline) output)) :separator ",")))
                           (list (parse-number:parse-real-number time)
                                 (if (and (equal top-correct "false")
                                          (equal recursive-top-correct "false"))
                                     "wrong"
                                     t))))
                        ((> retval 127) ; we probably manually killed the process
                         (list sb-ext:single-float-positive-infinity nil))
                        (t (list sb-ext:single-float-positive-infinity (string-trim '(#\Newline) error))))))
            (format *trace-output* "~a result: ~a ~a~%" symbol time correct-p)
            (if silent
                (list time correct-p examples)
                (push (list options time correct-p examples)
                      (gethash symbol *results*)))))))))
(setf (get '> :latex) "$>$")
