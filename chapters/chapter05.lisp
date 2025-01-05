;;; 5.1
(defun good-style (p)
  (let ((q (+ p 5)))
    (list 'result 'is q)))
;;; (good-style 8)

;;; 5.6 Dice
;;; a.
(defun throw-die ()
  (+ 1 (random 6)))
;;; (throw-die)
;;;
;;; b.
(defun throw-dice ()
  (list (throw-die) (throw-die)))
;;; (throw-dice)
;;;
;;; c.
(defun snake-eyes-p (throw)
  (= 1 (car throw) (cadr throw)))
;;; (snake-eyes-p '(1 1)) ; T
;;; (snake-eyes-p '(1 6)) ; NIL
;;; (snake-eyes-p '(3 1)) ; NIL
(defun boxcars-p (throw)
  (= 6 (car throw) (cadr throw)))
;;; (boxcars-p '(6 6)) ; T
;;; (boxcars-p '(2 4)) ; NIL
;;; (boxcars-p '(6 5)) ; NIL
;;;
;;; d.
(defun instant-win-p (throw)
  (let ((total (+ (car throw) (cadr throw))))
    (or (= 7 total)
        (= 11 total))))
;;; (instant-win-p '(3 4)) ; T
;;; (instant-win-p '(4 4)) ; NIL
(defun instant-loss-p (throw)
  (let ((total (+ (car throw) (cadr throw))))
    (or (= 2 total)
        (= 3 total)
        (= 12 total))))
;;; (instant-loss-p '(1 1)) ; T
;;; (instant-loss-p '(2 1)) ; T
;;; (instant-loss-p '(6 6)) ; T
;;; (instant-loss-p '(6 4)) ; NIL
;;;
;;; e.
(defun say-throw (throw)
  (cond ((snake-eyes-p throw) 'snake-eyes)
        ((boxcars-p throw) 'boxcars)
        (t (+ (car throw) (cadr throw)))))
;;; (say-throw '(1 1)) ; 'snake-eyes
;;; (say-throw '(6 6)) ; 'boxcars
;;; (say-throw '(3 4)) ; 7
;;;
;;; f.
(defun craps ()
  (let* ((throw (throw-dice))
         (die-1 (car throw))
         (die-2 (cadr throw))
         (said-throw (say-throw throw)))
    (cond ((instant-win-p throw) (list 'throw die-1 'and die-2 '-- said-throw '-- 'you 'win))
          ((instant-loss-p throw) (list 'throw die-1 'and die-2 '-- said-throw '-- 'you 'lose))
          (t (list 'throw die-1 'and die-2 '-- 'your 'point 'is said-throw)))))
;;;(craps)
;;;
;;;
;;; g.
(defun try-for-point (point)
  (let* ((throw (throw-dice))
         (die-1 (car throw))
         (die-2 (cadr throw))
         (total (+ die-1 die-2)))
    (cond ((= 7 total) (list 'throw die-1 'and die-2 '-- total '-- 'you 'lose))
          ((= point total) (list 'throw die-1 'and die-2 '-- 'you 'win))
          (t (list 'throw die-1 'and die-2 '-- total '-- 'throw 'again)))))
;;; (try-for-point 5)
