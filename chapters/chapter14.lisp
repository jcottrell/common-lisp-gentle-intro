;;; 14.1
;(setf x '(1 2 3))
;(macroexpand-1 '(pop x))
;;
;; result:
;; (let* ((#:list x) (#:car (car #:list)) (#:new276 (cdr #:list)))
;;   (setq x #:new276)
;;   #:car)
;; T

;;; 14.2
(macroexpand-1 '(defstruct starship
                  (name nil)
                  (condition 'green)))
;;
;; result:
;;(PROGN
;; (EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
;;   (SB-KERNEL::%DEFSTRUCT-PACKAGE-LOCKS
;;    '#<SB-KERNEL:DEFSTRUCT-DESCRIPTION STARSHIP {1006758343}>))
;; (SB-KERNEL::%DEFSTRUCT
;;  '#<SB-KERNEL:DEFSTRUCT-DESCRIPTION STARSHIP {1006758343}>
;;  '#(#<SB-KERNEL:LAYOUT for T {1000008003}>
;;     #<SB-KERNEL:LAYOUT (ID=1) for STRUCTURE-OBJECT {1000008083}>)
;;  (SB-C:SOURCE-LOCATION))
;; (EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
;;   (SB-KERNEL::%COMPILER-DEFSTRUCT
;;    '#<SB-KERNEL:DEFSTRUCT-DESCRIPTION STARSHIP {1006758343}>
;;    '#(#<SB-KERNEL:LAYOUT for T {1000008003}>
;;       #<SB-KERNEL:LAYOUT (ID=1) for STRUCTURE-OBJECT {1000008083}>)))
;; (SB-C:XDEFUN COPY-STARSHIP
;;     :COPIER
;;     NIL
;;     (SB-KERNEL:INSTANCE)
;;   (COPY-STRUCTURE (THE STARSHIP SB-KERNEL:INSTANCE)))
;; (SB-C:XDEFUN STARSHIP-P
;;     :PREDICATE
;;     NIL
;;     (SB-KERNEL::OBJECT)
;;   (TYPEP SB-KERNEL::OBJECT 'STARSHIP))
;; (SB-C:XDEFUN (SETF STARSHIP-NAME)
;;     :ACCESSOR
;;     (NAME NIL)
;;     (SB-KERNEL::VALUE SB-KERNEL:INSTANCE)
;;   (LET ((#:INSTANCE (THE STARSHIP SB-KERNEL:INSTANCE))
;;         (#:VAL SB-KERNEL::VALUE))
;;     (SB-KERNEL:%INSTANCE-SET #:INSTANCE 1 #:VAL)
;;     #:VAL))
;; (SB-C:XDEFUN STARSHIP-NAME
;;     :ACCESSOR
;;     (NAME NIL)
;;     (SB-KERNEL:INSTANCE)
;;   (SB-KERNEL:%INSTANCE-REF (THE STARSHIP SB-KERNEL:INSTANCE) 1))
;; (SB-C:XDEFUN (SETF STARSHIP-CONDITION)
;;     :ACCESSOR
;;     (CONDITION 'GREEN)
;;     (SB-KERNEL::VALUE SB-KERNEL:INSTANCE)
;;   (LET ((#:INSTANCE (THE STARSHIP SB-KERNEL:INSTANCE))
;;         (#:VAL SB-KERNEL::VALUE))
;;     (SB-KERNEL:%INSTANCE-SET #:INSTANCE 2 #:VAL)
;;     #:VAL))
;; (SB-C:XDEFUN STARSHIP-CONDITION
;;     :ACCESSOR
;;     (CONDITION 'GREEN)
;;     (SB-KERNEL:INSTANCE)
;;   (SB-KERNEL:%INSTANCE-REF (THE STARSHIP SB-KERNEL:INSTANCE) 2))
;; (SB-C:XDEFUN MAKE-STARSHIP
;;     :CONSTRUCTOR
;;     NIL
;;     (&KEY ((:NAME #:NAME) NIL)
;;      ((:CONDITION #:CONDITION)
;;       (SB-KERNEL:THE*
;;        (T :SOURCE-FORM (CONDITION 'GREEN) :CONTEXT
;;         #<SB-KERNEL:DEFSTRUCT-SLOT-DESCRIPTION CONDITION> :USE-ANNOTATIONS T)
;;        'GREEN)))
;;   (DECLARE (SB-INT:EXPLICIT-CHECK)
;;            (SB-C::LAMBDA-LIST
;;             (&KEY ((:NAME #:NAME) NIL)
;;              ((:CONDITION #:CONDITION) (QUOTE GREEN)))))
;;   (SB-KERNEL::%MAKE-STRUCTURE-INSTANCE-MACRO
;;    #<SB-KERNEL:DEFSTRUCT-DESCRIPTION STARSHIP {1006758343}>
;;    '((:SLOT T . 1) (:SLOT T . 2)) #:NAME #:CONDITION))
;; (SB-KERNEL::%TARGET-DEFSTRUCT
;;  '#<SB-KERNEL:DEFSTRUCT-DESCRIPTION STARSHIP {1006758343}>
;;  (SB-INT:NAMED-LAMBDA "STARSHIP-EQUALP"
;;      (SB-KERNEL::A SB-KERNEL::B)
;;    (DECLARE (OPTIMIZE (SB-C:STORE-SOURCE-FORM 0) (SAFETY 0))
;;             (TYPE STARSHIP SB-KERNEL::A SB-KERNEL::B)
;;             (IGNORABLE SB-KERNEL::A SB-KERNEL::B))
;;    (AND
;;     (EQUALP (TRULY-THE T (SB-KERNEL:%INSTANCE-REF SB-KERNEL::A 1))
;;             (TRULY-THE T (SB-KERNEL:%INSTANCE-REF SB-KERNEL::B 1)))
;;     (EQUALP (TRULY-THE T (SB-KERNEL:%INSTANCE-REF SB-KERNEL::A 2))
;;             (TRULY-THE T (SB-KERNEL:%INSTANCE-REF SB-KERNEL::B 2)))))
;;  '(SETF STARSHIP-NAME) 'STARSHIP-NAME '(SETF STARSHIP-CONDITION)
;;  'STARSHIP-CONDITION))
;;T

;;; 14.3
(defmacro set-nil (var)
  (list 'setf var 'nil))
;(setf x 1)
;(set-nil x)

;;; 14.4
(defmacro simple-rotate (left right)
  (let ((temp-storage (gensym)))
    `(let ((,temp-storage ,left))
       (setf ,left ,right)
       (setf ,right ,temp-storage))))
;(setf m 'n)
;(setf n 'm)
;(simple-rotate m n); m is now 'm and n is now 'n

;;; 14.5
(defmacro set-mutual (left right)
  `(progn (setf ,left ',right)
          (setf ,right ',left)))
;(set-mutual a b); a is 'b and b is 'a

;;; 14.6
(defmacro variable-chain (&rest xs)
    `(progn (setf ,(car xs) ',(cadr xs))
            (when (> (length ',(cdr xs)) 1)
              (variable-chain ,@(cdr xs)))))
;(variable-chain a b c d); a is 'b, b is 'c, c is 'd
