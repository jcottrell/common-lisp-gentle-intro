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

;;; 10.5
(defun ugly (x y)
  (let* ((larger (max x y))
         (avg (/ (+ x y) 2.0))
         (pct (* 100 (/ avg larger))))
    (list 'average avg 'is
          pct 'percent 'of 'max larger)))
;(ugly 20 2)

;;; 10.6
;(setf x nil)
;(push x x); '(((NIL) NIL) (NIL) NIL)

;;; 10.7
; Length isn't a position within x so it can't be set.

;;; Toolkit: BREAK and ERROR
(defun analyze-profit (price commission-rate)
  (let* ((commission (* price commission-rate))
         (result (cond ((> commission 100) 'rich)
                       ((< commission 100) 'poor))))
    (break "Value of RESULT is ~S and COMMISSION is ~S" result commission)
    (format t "~&I predict you will be ~S on a commission of ~A" result commission)
    result))
;(analyze-profit 2000 0.05)

;;; 10.8
;;; Tic-Tac-Toe

(defun make-board ()
  (list 'board 0 0 0 0 0 0 0 0 0))

;(setf b (make-board))

(defun convert-to-letter (position-value)
  (cond ((equal position-value 1) "O")
        ((equal position-value 10) "X")
        (t " ")))

(defun print-row (left middle right)
  (format t "~&  ~A | ~A | ~A~%"
          (convert-to-letter left)
          (convert-to-letter middle)
          (convert-to-letter right)))

(defun print-board (board)
  (print-row
   (nth 1 board) (nth 2 board) (nth 3 board))
  (format t "~& -----------")
  (print-row
   (nth 4 board) (nth 5 board) (nth 6 board))
  (format t "~& -----------")
  (print-row
   (nth 7 board) (nth 8 board) (nth 9 board))
  (finish-output))
;(print-board b)

(defun make-move (player pos board)
  (setf (nth pos board) player)
  board)

(setf *computer* 10)
(setf *opponent* 1)

;(make-move *opponent* 3 b)
;(make-move *computer* 5 b)
;(print-board b)

(setf *triplets*
      '((1 2 3) (4 5 6) (7 8 9) ; horizontal
        (1 4 7) (2 5 8) (3 6 9) ; vertical
        (1 5 9) (3 5 7))) ; diagonal

(defun sum-triplet (board triplet)
  (+ (nth (first triplet) board)
     (nth (second triplet) board)
     (nth (third triplet) board)))
;(sum-triplet b '(3 5 7)); 11
;(sum-triplet b '(2 5 8)); 10
;(sum-triplet b '(7 8 9)); 0

(defun compute-sums (board)
  (mapcar #'(lambda (triplet)
              (sum-triplet board triplet))
          *triplets*))
;(compute-sums b); '(1 10 0 0 10 1 10 11)

(defun winner-p (board)
  (let ((sums (compute-sums board)))
    (or (member (* 3 *computer*) sums)
        (member (* 3 *opponent*) sums))))
;(winner-p '(board 10 10 10 1 0 0 1 1 10))

(defun play-one-game ()
  (let ((board (make-board)))
    (print-board board)
    (if (y-or-n-p "Would you like to go first? ")
        (opponent-move board)
        (computer-move board))))

(defun opponent-move (board)
  (let* ((pos (read-a-legal-move board))
         (new-board (make-move
                     *opponent*
                     pos
                     board)))
    (print-board new-board)
    (cond ((winner-p new-board)
           (format t "~&You win!"))
          ((board-full-p new-board)
           (format t "~&Tie game."))
          (t (computer-move new-board)))))

(defun read-a-legal-move (board)
  (format t "~&Your move: ")
  (finish-output)
  (let ((pos (read)))
    (cond ((not (and (integerp pos)
                     (<= 1 pos 9)))
           (format t "~&Invalid input.")
           (read-a-legal-move board))
          ((not (zerop (nth pos board)))
           (format t "~&That space is already occupied.")
           (read-a-legal-move board))
          (t pos))))

(defun board-full-p (board)
  (not (member 0 board)))

(defun computer-move (board)
  (let* ((best-move (choose-best-move board))
         (pos (first best-move))
         (strategy (second best-move))
         (new-board (make-move *computer* pos board)))
    (format t "~&My move: ~S" pos)
    (format t "~&My strategy: ~A~%" strategy)
    (print-board new-board)
    (cond ((winner-p new-board)
           (format t "~&I win!"))
          ((board-full-p board)
           (format t "~&Tie game."))
          (t (opponent-move new-board)))))

(defun choose-best-move (board)
  (random-move-strategy board))

(defun random-move-strategy (board)
  (list (pick-random-empty-position board)
        "random move"))

(defun pick-random-empty-position (board)
  (let ((pos (+ 1 (random 9))))
    (if (zerop (nth pos board))
        pos
        (pick-random-empty-position board))))

(defun make-three-in-a-row (board)
  (let ((pos (win-or-block board
                           (* 2 *computer*))))
    (and pos (list pos "make three in a row"))))

(defun block-opponent-win (board)
  (let ((pos (win-or-block board
                           (* 2 *opponent*))))
    (and pos (list pos "block opponent"))))

(defun win-or-block (board target-sum)
  (let ((triplet (find-if
                  #'(lambda (trip)
                      (equal (sum-triplet board
                                          trip)
                             target-sum))
                  *triplets*)))
    (when triplet
      (find-empty-position board triplet))))

(defun find-empty-position (board squares)
  (find-if #'(lambda (pos)
               (zerop (nth pos board)))
           squares))

(defun choose-best-move (board)
  (or (make-three-in-a-row board)
      (block-opponent-win board)
      (block-squeeze-play board)
      (random-move-strategy board)))

;;; a.
(setf *corners* '(1 3 7 9))
(setf *sides* '(2 4 6 8))
;(find-empty-position b *sides*)

;;; b.
(setf *diagonals* '((1 5 9) (3 5 7)))

(defun block-squeeze-play (board)
  (let ((possible-squeeze (find-if #'(lambda (diagonal)
                                       (equalp (list (nth (first diagonal) board)
                                                     (nth (second diagonal) board)
                                                     (nth (third diagonal) board))
                                               (list *opponent* *computer* *opponent*)))
                                   *diagonals*)))
    (when possible-squeeze
      (list (find-empty-position board *sides*)
            "block squeeze play"))))
;(block-squeeze-play '(board 1 0 0 0 10 0 0 0 1)); '(2 "block squeeze play")
;(block-squeeze-play '(board 0 0 1 0 10 0 1 0 0)); '(2 "block squeeze play")

;;; c.
(defun block-two-on-one (board)
  (let ((possible (find-if #'(lambda (diagonal)
                               (or (equalp (list (nth (first diagonal) board)
                                                 (nth (second diagonal) board)
                                                 (nth (third diagonal) board))
                                           (list *opponent* *opponent* *computer*))
                                   (equalp (list (nth (first diagonal) board)
                                                 (nth (second diagonal) board)
                                                 (nth (third diagonal) board))
                                           (list *computer* *opponent* *opponent*))))
                           *diagonals*)))
    (when possible
      (let ((corner-move (find-empty-position board *corners*)))
        (when corner-move
          (list corner-move
                "block two on one"))))))

;;; d.
(defun choose-best-move (board)
  (or (make-three-in-a-row board)
      (block-opponent-win board)
      (block-squeeze-play board)
      (block-two-on-one board)
      (random-move-strategy board)))

;(block-two-on-one
; (list 'board
;       *opponent* 0          0
;       0          *opponent* 0
;       0          0          *computer*)); '(3 "block two on one")

;;; e.
(defun make-squeeze-play (board)
  (let* ((target-total (+ *computer* *opponent*))
         (possible (find-if #'(lambda (diagonal)
                                (let ((diagonal-value (+ (nth (first diagonal) board)
                                                         (nth (second diagonal) board)
                                                         (nth (third diagonal) board)))
                                      (middle-opponent (equalp *opponent* (nth (second diagonal) board))))
                                  (and (= diagonal-value target-total)
                                       middle-opponent)))
                            *diagonals*)))
    (when possible
      (let ((found-square (find-empty-position board possible)))
        (when found-square
          (list found-square "Make squeeze play"))))))
;(make-squeeze-play (list 'board 0 0 *computer* 0 *opponent* 0 0 0 0)); '(7 "Make squeeze play")
;(make-squeeze-play (list 'board *computer* 0 0 0 0 0 0 0 *opponent*)); nil

(defun make-two-on-one (board)
  (let ((possible (find-if #'(lambda (diagonal)
                               (and (equalp *computer* (nth (second diagonal) board))
                                    (or (equalp *opponent* (nth (first diagonal) board))
                                        (equalp *opponent* (nth (third diagonal) board)))
                                    (not (equalp (nth (first diagonal) board)
                                                 (nth (third diagonal) board)))))
                           *diagonals*)))
    (when possible
      (list (find-empty-position board possible) "Make two on one"))))
;(make-two-on-one (list 'board 0 0 *opponent* 0 *computer* 0 0 0 0)); '(7 "Make two on one")

(defun choose-best-move (board)
  (or (make-three-in-a-row board)
      (block-opponent-win board)
      (block-squeeze-play board)
      (block-two-on-one board)
      (make-squeeze-play board)
      (make-two-on-one board)
      (random-move-strategy board)))

;(computer-move '(board 10 10 1 1 1 10 10 0 1))
;(choose-best-move '(board 10 10 1 1 1 10 10 0 1))
;
;(make-three-in-a-row '(board 10 10 1 1 1 10 10 0 1)); nil
;(block-opponent-win '(board 10 10 1 1 1 10 10 0 1)); nil
;(block-squeeze-play '(board 10 10 1 1 1 10 10 0 1)); nil
;(block-two-on-one '(board 10 10 1 1 1 10 10 0 1)); nil
;(make-squeeze-play '(board 10 10 1 1 1 10 10 0 1)); nil
;(make-two-on-one '(board 10 10 1 1 1 10 10 0 1)); nil
;(random-move-strategy '(board 10 10 1 1 1 10 10 0 1)); '(8 "random move")

;;; 10.9
(defun chop (whole)
  (setf (cdr whole) '()))
;(setf *whole* '(fee fie foe fum))
;(chop *whole*); NIL; whole is also '(fee)

;;; 10.10
(defun ntack (starting to-tack)
  (setf (cdr (last starting)) (cons to-tack '())))

;;; 10.11
;'(a b c a b c ...)

;;; 10.12
; The first gives you a new appended list of four.
; The second gives you infinite recursion.
