; 4.7
(defun small-positive-oddp (x)
  (and (< x 100)
       (> x 0)
       (oddp x)))
; (small-positive-oddp 5)

; 4.9 exercises
;; 4.15
(defun geq (left right)
  (>= left right))
; (geq 5 2)
; (geq 4 10)
;
;; 4.16
(defun square-double-divide (x)
  (cond ((and (oddp x)
              (> x 0)) (* x x))
        ((and (oddp x)
              (< x 0)) (* 2 x))
        (t (/ x 2))))
; (square-double-divide 5) ;; 25
; (square-double-divide -5);; -10
; (square-double-divide 6) ;; 3
;
; use only AND and OR
(defun square-double-divide-logic (x)
  (or (and (oddp x)
           (or (and (> x 0)
                    (* x x))
               (and (< x 0)
                    (* 2 x))))
      (/ x 2)))
; (square-double-divide-logic 5) ;; 25
; (square-double-divide-logic -5);; -10
; (square-double-divide-logic 6) ;; 3
;
;; 4.17
(defun check-gender-name (gender name)
  (or (and (equal gender 'boy)
           (equal name 'child))
      (and (equal gender 'girl)
           (equal name 'child))
      (and (equal gender 'man)
           (equal name 'adult))
      (and (equal gender 'woman)
           (equal name 'adult))))
; (check-gender-name 'boy 'child) ;; T
; (check-gender-name 'girl 'child) ;; T
; (check-gender-name 'man 'adult) ;; T
; (check-gender-name 'woman 'adult) ;; T
; (check-gender-name 'boy 'adult) ;; NIL
; (check-gender-name 'girl 'adult) ;; NIL
;
; 4.18
(defun play (first-player-choice second-player-choice)
  (cond ((or (and (equal first-player-choice 'rock)
                  (equal second-player-choice 'scissors))
             (and (equal first-player-choice 'paper)
                  (equal second-player-choice 'rock))
             (and (equal first-player-choice 'scissors)
                  (equal second-player-choice 'paper)))
         'first-wins)
        ((or (and (equal second-player-choice 'rock)
                  (equal first-player-choice 'scissors))
             (and (equal second-player-choice 'paper)
                  (equal first-player-choice 'rock))
             (and (equal second-player-choice 'scissors)
                  (equal first-player-choice 'paper)))
         'second-wins)
        (t 'tie)))
; (play 'rock 'scissors) ;; 'first-wins
; (play 'paper 'scissors) ;; 'second-wins
; (play 'rock 'rock) ;; 'tie
;
; 4.19
(defun cond-and (x y z w)
  (cond (x (cond (y (cond (z w)))))))
(defun if-and (x y z w)
  (if x
      (if y
          (if z
              (if w w
                  nil)
              nil)
          nil)
      nil))
; (cond-and 5 'j 0 "thing")
; (if-and 5 'j 0 "thing")
;
; 4.20
(defun compare-if (x y)
  (if (= x y)
      'numbers-are-the-same
      (if (< x y)
          'first-is-smaller
          (if (> x y)
              'first-is-bigger
              nil))))
(defun compare-and-or (x y)
  (or (and (= x y) 'numbers-are-the-same)
      (and (< x y) 'first-is-smaller)
      (and (> x y) 'first-is-bigger)))
; (compare-if 5 5)
; (compare-if 4 5)
; (compare-if 6 5)
; (compare-and-or 5 5)
; (compare-and-or 4 5)
; (compare-and-or 6 5)
;
; 4.21
(defun gtest-if (x y)
  (if (> x y)
      t
      (if (zerop x)
          t
          (zerop y))))
; (gtest-if 5 4) ; T
; (gtest-if 0 4) ; T
; (gtest-if 5 0) ; T
; (gtest-if 5 6) ; NIL
(defun gtest-cond (x y)
  (cond ((> x y) t)
        ((zerop x) t)
        (t (zerop y))))
; (gtest-cond 5 4) ; T
; (gtest-cond 0 4) ; T
; (gtest-cond 5 0) ; T
; (gtest-cond 5 6) ; NIL
;
; 4.22
(defun boilingp (temp scale)
  (cond ((equal scale 'fahrenheit) (> temp 212))
        ((equal scale 'celsius) (> temp 100))))
; (boilingp 215 'fahrenheit) ; T
; (boilingp 150 'fahrenheit) ; NIL
; (boilingp 115 'celsius) ; T
; (boilingp 50 'celsius) ; NIL
(defun boilingp-if (temp scale)
  (if (equal scale 'fahrenheit)
      (> temp 212)
      (if (equal scale 'celsius)
          (> temp 100)
          nil)))
; (boilingp-if 215 'fahrenheit) ; T
; (boilingp-if 150 'fahrenheit) ; NIL
; (boilingp-if 115 'celsius) ; T
; (boilingp-if 50 'celsius) ; NIL
(defun boilingp-and-or (temp scale)
  (or (and (equal scale 'fahrenheit)
           (> temp 212))
      (and (equal scale 'celsius)
           (> temp 100))))
; (boilingp-and-or 215 'fahrenheit) ; T
; (boilingp-and-or 150 'fahrenheit) ; NIL
; (boilingp-and-or 115 'celsius) ; T
; (boilingp-and-or 50 'celsius) ; NIL

; 4.28
(or (and (not (oddp 5)) 'foo)
    (evenp 7))

; Advanced Topics
(defun logical-and (x y)
  (and x y t))
; (logical-and 'tweet 'woof) ; T
; (and 'tweet 'woof) ; 'woof

; 4.29
(defun logical-and-if (x y)
  (if x
      (if y
          t)))
; (logical-and-if 'tweet 'woof)
; (logical-and-if nil 'woof)
(defun logical-and-cond (x y)
  (cond (x (cond (y t)))))
; (logical-and-cond 'tweet 'woof) ; T
; (logical-and-cond nil 'woof) ; NIL

; 4.30
(defun logical-or (x y)
  (or (and x t)
      (and y t)))
; (logical-or 'tweet nil) ; T
; (logical-or nil t) ; T
; (logical-or (oddp 5) (oddp 2)) ; T
; (logical-or nil nil) ; NIL

; 4.31
; No
; (not 'tweet) ; NIL
; (not nil) ; T

(defun demorgan-and (x y)
  (not (or (not x) (not y))))

(defun demorgan-or (x y)
  (not (and (not x) (not y))))

; 4.35
(defun demorgan-and-3 (x y z)
  (not (or (not x) (not y) (not z))))
(defun demorgan-or-3 (x y z)
  (not (and (not x) (not y) (not z))))

; 4.36
;
(defun nand (x y)
  (not (and x y)))
; (nand t t) ; NIL
; (nand t nil) ; T
; (nand nil t) ; T
; (nand nil nil) ; T

; 4.37
(defun not2 (x)
  (nand x x))
; (not2 t) ; NIL
; (not2 nil) ; T
