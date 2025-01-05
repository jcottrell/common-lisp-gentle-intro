;;; Assignment
;

(setf *total-glasses* 0)

(defun sell (number-of-sold-glasses-this-round)
  "Ye Olde Lemonade Stand: Sales by the Glass."
  (setf *total-glasses* (+ *total-glasses* number-of-sold-glasses-this-round))
  (format t
          "~&That makes ~S glasses so far today."
          *total-glasses*))

;;; 10.1
; Guess:  We would get an error if we didn't set *total-glasses* before calling
;         sell.
; Actual: There was an error that sent us to the debugger because
;         "*total-glasses* is unbound.". I was able to add a value at the
;         debugger (zero) so it worked afterward.
; Guess:  We would get an error about adding a number to a symbol.
; Actual: There was an error that sent us to the debugger because the value 'foo
;         is not of type NUMBER.

;;; 10.2
(defun sell-ink (newly-sold-glasses)
  (progn (incf *total-glasses* newly-sold-glasses)
         (format t
                 "~&That makes ~S glasses so far today."
                 *total-glasses*)))

;;; 10.3
(setf *friends* '())
(setf *previously-met* 0)

(defun meet (person)
  (cond ((member person *friends*)
         (progn (incf *previously-met*)
                (cond ((equal person (first *friends*))
                       'we-just-met)
                      (t 'we-know-each-other))))
        (t (progn (push person *friends*)
                  'pleased-to-meet-you))))
;(meet 'fred); 'pleased-to-meet-you
;(meet 'cindy); 'pleased-to-meet-you
;(meet 'cindy); 'we-just-met
;(meet 'joe); 'pleased-to-meet-you
;(meet 'fred); 'we-know-each-other
;*friends*; '(joe cindy fred)
;*previously-met*; 2

;;; 10.4
(defun forget (person)
  (cond ((member person *friends*)
         (progn (setf *friends* (remove-if #'(lambda (friend) (equal friend person))
                                           *friends*))
                'i-no-longer-remember-that-person))
        (t 'how-can-i-forget-someone-i-have-not-met)))
