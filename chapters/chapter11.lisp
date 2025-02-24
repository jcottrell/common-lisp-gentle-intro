;;; Chapter 11

;;; 11.1
(defun it-member (item things)
  (dolist (c things)
    (if (equalp c item)
        (return t))))
;(it-member 'a '(d c b a zero))
;(it-member 'a '(b c d e f))

;;; 11.2
(defun it-assoc (item alist)
  (dolist (pair alist)
    (if (equalp item (car pair))
        (return pair))))
;(it-assoc 'a '((b more-things) (a others) (c thirds)))

;;; 11.3
(defun check-all-odd (numbers &optional (good-so-far t))
  (cond ((not good-so-far) nil)
        ((null numbers) good-so-far)
        (t (progn (format t "~&Checking ~S..." (car numbers))
                  (check-all-odd (cdr numbers) (oddp (car numbers)))))))
;(check-all-odd '(1 3 5 7 2 1))

;;; 11.4
(defun it-length (things)
  (let ((counter 0))
    (dolist (thing things counter)
      (setf counter (1+ counter)))))

;;; 11.5
(defun it-nth (n things)
  (let ((counter 0))
    (dolist (thing things)
      (if (= n counter)
          (return thing)
          (setf counter (1+ counter))))))

;;; 11.6
(defun it-union (left right)
  (let ((result '()))
    (dolist (piece left (append result right))
     (if (not (member piece right))
         (setf result (cons piece result))))))

;;; 11.7
; The push function pushes the item onto the front of the list so the first
; match ends up the last in the list.
(defun it-interesection (left right)
  (let ((result '()))
    (dolist (piece left result)
      (when (member piece right)
        (setf result (append result (list piece)))))))

;;; 11.8
(defun it-reverse (tsil)
  (let ((reversed '()))
    (dolist (item tsil reversed)
      (setf reversed (cons item reversed)))))

;;; 11.9
(defun check-all-odd (mixed)
  (do ((remaining (cdr mixed) (cdr remaining))
       (item (car mixed) (car remaining)))
      ((progn (format t "~&Checking ~S..." item)
              (null remaining))
       (format t "~&All odd.") t
       (when (evenp item)
           (return nil)))))

;;; 11.10
(defun launch-times (high-num)
  (format t "~%")
  (dotimes (current high-num (format t "Blast off!"))
    (format t "~A..." (- high-num current))))

;;; 11.11
(defun find-largest-do* (list-of-numbers)
  (do* ((remaining list-of-numbers (cdr remaining))
        (current (car remaining) (car remaining))
        (largest (first list-of-numbers) (if (and (not (null current))
                                                  (> current largest))
                                             current
                                             largest)))
       ((null remaining) largest)))

;;; 11.12
(defun power-of-2-do* (power)
  (do* ((current-power 0 (1+ current-power))
        (total (if (zerop power) 0 1) (* 2 total)))
       ((= power current-power) total)))

;;; 11.13
(defun first-non-integer (xs)
  "Return the first non-integer element of XS"
  (dolist (search xs 'none)
    (when (not (integerp search))
      (return search))))

;;; 11.14
; It would complain about x not being bound in (first x)

;;; 11.15
; The last number isn't checked because the (null z) is checked before the (if (oddp e) (return e))

;;; 11.16
; LET has two parts; DO can have either two or three parts; LET executes onece;
; DO executes each time through the loop.

;;; 11.17
; The number 5 is returned becasue i is repeated in the first parenthesis of DO.

;;; 11.18
(defun do-version-of-11.17 ()
  (do ((i 0 (+ i 1)))
      ((= i 5) i)
    (format t "~&I = ~S" i)))
; Sure

;;; 11.19
; No switching the values in a DO does not make a difference. It doesn't make a
; difference because they are not allowed to interact. If there was a setf in
; them then it might but not normally.

;;; 11.20
; Yes

;;; 11.21
(defun it-fibo (target)
  (do* ((i 0 (+ i 1))
        (left 0 right)
        (right 1 next)
        (next (+ left right) (+ left right)))
       ((= i target) left)))

;;;
;;; Keyboard Exercise
;;;

;;; 11.22
;;;
;;; a.
(defun complement-base (base)
  (cond ((equalp base 'a) 't)
        ((equalp base 't) 'a)
        ((equalp base 'g) 'c)
        ((equalp base 'c) 'g)))
;(complement-base 't)

;;; b.
(defun complement-strand (strand)
  (mapcar #'complement-base strand))
;(complement-strand '(a g g t))

;;; c.
(defun make-double (strand)
  (mapcar #'list strand (complement-strand strand)))
;(make-double '(g g a c t)); '((g c) (g c) (a t) (c g) (t a))

;;; d.
;(defun count-bases (stranded)
;  (count-bases-recur stranded '((a 0) (t 0) (g 0) (c 0))))
;(defun count-bases-recur (remaining current-totals)
;  (cond ((null remaining) current-totals)
;        ((consp remaining) (count-bases-recur (cdr remaining)
;                                              (count-bases-recur (car remaining) current-totals)))
;        (t (progn (setf (second (assoc remaining current-totals))
;                        (1+ (second (assoc remaining current-totals))))
;                  current-totals))))
(defun add-to-totals (item alist)
  (let* ((current-value (second (assoc item alist)))
         (updated-pair (list item (1+ current-value))))
    (mapcar (lambda (pair)
              (if (equalp item (car pair))
                  updated-pair
                  pair))
            alist)))
;(add-to-totals 'a '((a 5) (b 2) (c 1)))
(defun count-bases (stranded &optional (totals '((a 0) (t 0) (g 0) (c 0))))
  (cond ((null stranded) totals)
        ((consp stranded) (count-bases (cdr stranded)
                                       (count-bases (car stranded) totals)))
        (t (add-to-totals stranded totals))))
;(count-bases '((g c) (a t) (t a) (t a) (c g))); '((a 3) (t 3) (g 2) (c 2))
;(count-bases '(a g t a c t c t)); '((a 2) (t 3) (g 1) (c 2))

;;; e.
(defun prefixp (possible whole)
  (cond ((null possible) t)
        ((null whole) nil)
        ((equalp (car possible)
                 (car whole))
         (prefixp (cdr possible) (cdr whole)))
        (t nil)))
;(prefixp '(g t c) '(g t c a t)); T
;(prefixp '(g t c) '(a g g t c)); NIL
;(prefixp '(c a t) '()); NIL

;;; f.
(defun appearsp (possible whole)
  (and (not (null whole))
       (or (prefixp possible whole)
           (appearsp possible (cdr whole)))))
;(appearsp '(c a t) '(t c a t g)); T
;(appearsp '(c a t) '(t c c g t a)); NIL

;;; g.
(defun drop (number things)
  (cond ((zerop number) things)
        (t (drop (1- number) (cdr things)))))
(defun coverp (pattern whole)
  (or (null whole)
      (and (not (null pattern))
           (prefixp pattern whole)
           (coverp pattern (drop (length pattern) whole)))))
;(coverp '(a g c) '(a g c a g c a g c)); T
;(coverp '(a g c) '(a g c t t g)); NIL

;;; h.
(defun prefix (to-take things &optional (result '()))
  (cond ((zerop to-take) result)
        ((> to-take (length things)) nil)
        (t (prefix (1- to-take) (cdr things) (append result (list (car things)))))))
;(prefix 4 '(c g a t t a g)); '(c g a t)

;;; i.
(defun kernel (whole &optional (separator-index 1))
  (let ((part (prefix separator-index whole)))
    (cond ((> separator-index (1+ (/ (length whole) 2))) NIL)
          ((coverp part whole) part)
          (t (kernel whole (1+ separator-index))))))
;(kernel '(a g c a g c a g c)); '(a g c)
;(kernel '(a a a a a a a)); '(a)
;(kernel '(a g g t c)); NIL

;;; j.
(defun draw-border (strand-length)
  (format t "~%")
  (dotimes (x strand-length)
    (format t "-----"))
  (format t "---"))
(defun draw-strand (strand)
  (format t "~%")
  (dolist (base strand)
    (format t "    ~A" base)))
(defun draw-separator (strand-length)
  (format t "~%")
  (dotimes (x strand-length)
    (format t "    .")))
(defun draw-dna (single-strand)
  (draw-border (length single-strand))
  (draw-strand single-strand)
  (draw-separator (length single-strand))
  (draw-separator (length single-strand))
  (draw-strand (complement-strand single-strand))
  (draw-border (length single-strand)))
