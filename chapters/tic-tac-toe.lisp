;;; Tic-Tac-Toe
;;;
;;; Run at the terminal with:
;;; sbcl --script chapters/tic-tac-toe.lisp

(defparameter *computer* 10)
(defparameter *opponent* 1)

(defparameter *triplets*
      '((1 2 3) (4 5 6) (7 8 9) ; horizontal
        (1 4 7) (2 5 8) (3 6 9) ; vertical
        (1 5 9) (3 5 7))) ; diagonal
(defparameter *corners* '(1 3 7 9))
(defparameter *sides* '(2 4 6 8))
(defparameter *diagonals* '((1 5 9) (3 5 7)))

(defun make-board ()
  (list 'board 0 0 0 0 0 0 0 0 0))

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

(defun make-move (player pos board)
  (setf (nth pos board) player)
  board)

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

;(block-two-on-one (list 'board *opponent* 0 0 0 *opponent* 0 0 0 *computer*)); '(3 "block two on one")

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

(play-one-game)
