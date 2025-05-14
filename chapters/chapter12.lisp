(load "~/quicklisp/setup.lisp")
(ql:quickload "prompt-for")
;; see 12.5, commented out to do that
;(defstruct starship
;  (captain nil)
;  (name nil)
;  (shields 'down)
;  (condition 'green)
;  (speed 0))
;;; 12.1
; The CAPTAIN is defined in the struct as the definition of the field.
; The :CAPTAIN is used when constructing the struct to specify its value.
; The STARSHIP-CAPTAIN is a function that takes an instance of a STARSHIP and returns its captain.

;;; 12.2
; No, (STARSHIP-P 'STARSHIP) is checking a symbol, not an instance of a starship.

;;; 12.3
;(type-of 'make-starship); SYMBOL
;(type-of #'make-starship); COMPILED-FUNCTION
;(type-of (make-starship)); STARSHIP

;;; Keyboard exercise

;;; 12.4
;;; a.
(defstruct node
  (name nil)
  (question nil)
  (yes-case nil)
  (no-case nil))

;;; b.
(setf *node-list* nil)

(defun init ()
  (setf *node-list* nil))

;;; c.
(defun add-node (name question yes-branch no-branch)
  (let ((created-node (make-node :name name
                                 :question question
                                 :yes-case yes-branch
                                 :no-case no-branch)))
    (progn (setf *node-list* (append *node-list* (list (list name created-node))))
           name)))

;;; d.
(defun find-node (search-name)
  (assoc search-name *node-list*))

(add-node 'start
          "Does the engine turn over?"
          'engine-turns-over
          'engine-wont-turn-over)
(add-node 'engine-turns-over
          "Will the engine run for any period of time?"
          'engine-will-run-briefly
          'engine-wont-run)
(add-node 'engine-wont-run
          "Is there gas in the tank?"
          'gas-in-tank
          "Fill the tank and try starting the engine again.")
(add-node 'engine-wont-turn-over
          "Do you hear any sound when you turn the key?"
          'sound-when-turn-key
          'no-sound-when-turn-key)
(add-node 'no-sound-when-turn-key
          "Is the battery voltage low?"
          "Replace the battery"
          'battery-voltage-ok)
(add-node 'battery-voltage-ok
          "Are the battery cables dirty or loose?"
          "Clean the cables and tighten the connections."
          'battery-cables-good)

;;; e.
(defun process-node (name)
  (let ((found-node (second (find-node name))))
    (cond ((null found-node) (progn (format t "No node for ~A was defined yet." name)
                                    nil))
          (t (if (yes-or-no-p "~&~A" (node-question found-node))
                 (node-yes-case found-node)
                 (node-no-case found-node))))))

;;; f.
(defun run (&optional (current-node 'start))
  (let* ((result (process-node current-node)))
    (cond ((null result) nil)
          ((stringp result) (format t "~&~A" result))
          (t (run result)))))
;(run)

;;; g.
(defun get-input (question)
      (progn (format t "~%~A~%" question)
             (read-line)))
(defun str-or-sym-p (something)
  (or (symbolp something)
      (stringp something)))
(defun node-input ()
  (let ((name (prompt-for:prompt-for #'symbolp "Name?"))
        (question (prompt-for:prompt-for #'stringp "Question?"))
        (yes-path (prompt-for:prompt-for #'str-or-sym-p "Which node when the answer is yes?"))
        (no-path (prompt-for:prompt-for  #'str-or-sym-p "Which node when the answer is no?")))
    (add-node name question yes-path no-path)))
;; The above takes normal input for symbols and input wrapped in quotes for strings.

;;; 12.5
(defun print-starship (x stream depth)
  (format stream "#<STARSHIP ~A>"
          (starship-name x)))
(defstruct (starship
            (:print-function print-starship))
  (captain nil)
  (name nil)
  (shields 'down)
  (condition 'green)
  (speed 0))
(defun print-captain (c stream depth)
  (format stream "#<CAPTAIN ~A>"
          (captain-name c)))
(defstruct (captain
            (:print-function print-captain))
  (name "")
  (age nil)
  (ship nil))
(setf *enterprise* (make-starship :name "Enterprise"
                                  :shields 'up))
(setf *kirk* (make-captain :name "James T. Kirk"
                           :age 35
                           :ship *enterprise*))
(setf (starship-captain *enterprise*) *kirk*)
