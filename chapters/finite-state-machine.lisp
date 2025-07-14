;;
;; Chapter 14: Case Study
;;

(defun add-node (name)
  (let ((new-node (make-node :name name)))
    (setf *nodes* (nconc *nodes* (list new-node)))
    new-node))

(defmacro defnode (name)
  `(add-node ',name))

(defun add-arc (from-name label to-name action)
  (let* ((from (find-node from-name))
         (to (find-node to-name))
         (new-arc (make-arc :from from
                            :label label
                            :to to
                            :action action)))
    (setf *arcs* (nconc *arcs* (list new-arc)))
    (setf (node-outputs from)
          (nconc (node-outputs from)
                 (list new-arc)))
    (setf (node-inputs to)
          (nconc (node-inputs to)
                 (list new-arc)))
    new-arc))

(defmacro defarc (from label to &optional action)
  `(add-arc ',from ',label ',to ',action))

(defun print-node (node stream depth)
  (format stream "#<Node ~A>"
          (node-name node)))

(defstruct (node (:print-function print-node))
  (name nil)
  (inputs nil)
  (outputs nil))

(defun print-arc (arc stream depth)
  (format stream "#ARC ~A / ~A / ~A>"
          (node-name (arc-from arc))
          (arc-label arc)
          (node-name (arc-to arc))))

(defstruct (arc (:print-function print-arc))
  (from nil)
  (to nil)
  (label nil)
  (action nil))

(defvar *nodes*)
(defvar *arcs*)
(defvar *current-node*)

(defun initialize ()
  (setf *nodes* nil)
  (setf *arcs* nil)
  (setf *current-node* nil))

(defun find-node (name)
  (or (find name *nodes* :key #'node-name)
      (error "No node named ~A exists." name)))
;(find-node 'have-5); why not write a macro to remove the ' here?
;; TODO explore writing a macro find-node that will accept the name without the
;;      initial quote. Note: in finite-state-machine it is called without a
;;      quote.

(defun finite-state-machine (&optional (starting-point 'start))
  (setf *current-node* (find-node starting-point))
  (do ()
      ((null (node-outputs *current-node*)))
    (one-transition)))

(defun one-transition ()
  (format t "~&State ~A. Input: "
          (node-name *current-node*))
  (let* ((ans (read))
         (arc (find ans
                    (node-outputs *current-node*)
                    :key #'arc-label)))
    (unless arc
      (format t "~&No arc from ~A has label ~A.~%"
              (node-name *current-node*)
              ans)
      (return-from one-transition nil))
    (let ((new (arc-to arc)))
      (format t "~&~A" (arc-action arc))
      (setf *current-node* new))))

;; Need to define *nodes* and *arcs* before these macro calls otherwise I get
;; errors (warnings?) like The variable *nodes* is unbound.
;; 1. Run all the defvar lines above.
;; 2. (initialize)
;; 3. Run the commented-out lines below.

;(initialize)

;; Figure 14-2
;(defnode start)
;(defnode have-5)
;(defnode have-10)
;(defnode have-15)
;(defnode have-20)
;(defnode end)
;
;(defarc start nickel have-5 "Clunk!")
;(defarc start dime have-10 "Clink!")
;(defarc start coin-return start "Nothing to return.")
;
;(defarc have-5 nickel have-10 "Clunk!")
;(defarc have-5 dime have-15 "Clink!")
;(defarc have-5 coin-return start "Returned five cents.")
;
;(defarc have-10 nickel have-15 "Clunk!")
;(defarc have-10 dime have-20 "Clink!")
;(defarc have-10 coin-return start "Returned ten cents.")
;
;(defarc have-15 nickel have-20 "Clunk!")
;(defarc have-15 dime have-20 "Nickel change.")
;(defarc have-15 gum-button end "Deliver gum.")
;(defarc have-15 coin-return start "Returned fifteen cents.")
;
;(defarc have-20 nickel have-20 "Nickel returned.")
;(defarc have-20 dime have-20 "Dime returned.")
;(defarc have-20 gum-button end "Deliver gum, nickel change.")
;(defarc have-20 mint-button end "Deliver mints.")
;(defarc have-20 coin-return start "Returned twenty cents.")
