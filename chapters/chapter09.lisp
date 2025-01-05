;;; Format
;
; ~% is newline
; ~& is newline unless already at the beginning of a line
; ~S is an s-expression (with an extra for each one)
; ~A is an s-expression without escape characters (like double quote marks) but
;    list parenthesis are still printed.

;;; 9.1
(defun pilots ()
  (format t "~&There are old pilots,~&and there are bold pilots,~&but there are no old bold pilots."))

;;; 9.2
(defun draw-line (times)
  (labels ((draw-stars (remaining result)
                       (cond ((zerop remaining) result)
                             (t (draw-stars (- remaining 1)
                                            (concatenate 'string "*" result))))))
    (format t (draw-stars times ""))))
;(draw-line 5)
(defun draw-line-proc (times)
  (cond ((zerop times) (format t "~%"))
        (t (format t "*")
           (draw-line-proc (- times 1)))))
;(draw-line-proc 5)

;;; 9.3
(defun draw-box (width height)
  (cond ((zerop height) nil)
        (t (draw-line-proc width)
           (draw-box width (- height 1)))))
;(draw-box 10 4)
(defun get-line (width &optional (repeater "*"))
  (labels ((add-another (remaining so-far)
             (cond ((zerop remaining) so-far)
                   (t (add-another (- remaining 1) (concatenate 'string repeater so-far))))))
    (add-another width "")))
(defun draw-line-cleaner (width)
  (format t "~&~A~%" (get-line width)))
;(draw-line-cleaner 5)
(defun get-box (width height &optional (repeater "*"))
  (let ((one-line (get-line width repeater)))
    (labels ((add-box-row (remaining so-far)
                (cond ((zerop remaining) so-far)
                      (t (add-box-row (- remaining 1)
                                      (concatenate 'string so-far one-line "~%"))))))
      (add-box-row height ""))))
(defun draw-box-cleaner (width height)
  (format t (get-box width height)))
;(draw-box-cleaner 10 4)

;;; 9.4
(defun bottles-of-beer-song (total)
  (let* ((current-plural/singular (if (= total 1) "bottle" "bottles"))
         (next-total (- total 1))
         (next-plural/singular (if (= next-total 1) "bottle" "bottles")))
    (cond ((zerop total) (format t "Time to buy more beer for the wall"))
          (t (format t "~&~A ~A of beer on the wall,~%~A ~A of beer!~%Take one down,~%Pass it around,~%~A ~A of beer on the wall.~%~%"
                     total current-plural/singular
                     total current-plural/singular
                     (if (zerop next-total) "No more" next-total)
                     next-plural/singular)
             (bottles-of-beer-song (- total 1))))))
;(bottles-of-beer-song 3)

;;; 9.5
(defun print-board (raw-board)
  (let ((board (mapcar (lambda (piece) (if (null piece) " " piece)) raw-board)))
    (format t "~% ~a | ~a | ~a ~%-----------~% ~a | ~a | ~a ~%-----------~% ~a | ~a | ~a~%"
            (first board) (second board) (third board)
            (fourth board)  (fifth board) (sixth board)
            (seventh board) (eighth board) (ninth board))))
;(print-board '( x o o nil x nil 0 nil x))

;;; 9.6
(defun calculate-gross-pay (wage hours)
  (* wage hours 1.0))
(defun request-wage ()
  (progn (format t "~&How much do you make each hour you work? ")
         (let ((wage (read)))
           (cond ((numberp wage) wage)
                 (t (progn (format t "Invalid wage.~%")
                           (request-wage)))))))
(defun request-hours ()
  (progn (format t "~&How many hours did you work? ")
         (let ((hours (read)))
           (cond ((numberp hours) hours)
                 (t (progn (format t "Invalid number of hours.~%")
                           (request-hours)))))))
(defun request-gross-pay ()
  (let* ((wages (request-wage))
         (hours (request-hours))
         (total (calculate-gross-pay wages hours)))
    (format t "~&For an hourly wage of ~a at ~a / hr you're total is: $~a."
            wages hours total)))
;(request-gross-pay)

;;; 9.7
(defun cookie-monster ()
  (format t "~&Give me a cookie!!!~%")
  (format t "Cookie? ")
  (let ((object (read)))
    (if (equalp 'cookie object)
        (format t "Thank you!...Munch munch munch...~%")
        (progn (format t "No want ~a ...~%" object)
               (cookie-monster)))))
;(cookie-monster)

(defun get-tree-data-summary (input-file-path &optional (output t))
  (with-open-file (stream input-file-path)
    (let ((tree-location (read stream))
          (tree-table (read stream))
          (tree-total (read stream)))
      (format output "~&There are ~s trees on ~s."
              tree-total tree-location)
      (format output "~&They are: ~s" tree-table))))
;(get-tree-data-summary "./timber.dat")

(defun write-tree-summary (input-file-path output-file-path)
  (with-open-file (stream output-file-path
                          :direction :output
                          :if-exists :supersede)
    (get-tree-data-summary input-file-path stream)))
;(write-tree-summary "./timber.dat" "./timber-summary.txt")

;;; 9.8
; Strings can be copied into smaller string or queried but symbols are a whole
; unit of information that can't be broken down. There may be multiple strings
; but only one copy of a symbol exists in the program's memory.

;;; 9.9
; aB
; always
; broke
;
; ALPHABET

;;; 9.10
(defun negativep (n)
  (< n 0))
;(negativep 5) ; NIL
;(negativep 0) ; NIL
;(negativep -5); T

;;; a.
(defun space-over (spaces-to-print)
  (cond ((zerop spaces-to-print) nil)
        ((negativep spaces-to-print) (format t "Error!"))
        (t (progn (format t " ")
                  (space-over (- spaces-to-print 1))))))
(defun test (n)
  (format t "~%>>>")
  (space-over n)
  (format t "<<<"))
;(test 5)
;(test -5)

;;; b.
(defun plot-one-point (plotting-string y-val)
  (progn (format t "~&")
         (space-over y-val)
         (format t "~A~%" plotting-string)))
;(plot-one-point "*" 5)

;;; c.
(defun plot-points (plotting-string y-values)
  (cond ((zerop (length y-values)) nil)
        ((negativep (car y-values)) (format t "Error: negative y value: ~A" (car y-values)))
        (t (progn (plot-one-point plotting-string (car y-values))
                  (plot-points plotting-string (cdr y-values))))))
;(plot-points "<>" '(4 6 8 10 8 6 4))

;;; d.
(defun generate (start-number end-number)
  "Create a list of numbers from START-NUMBER to END-NUMBER, inclusive"
  (cond ((> start-number end-number) '())
        (t (cons start-number
                 (generate (+ 1 start-number) end-number)))))
;(generate -3 3); '(-3 -2 -1 0 1 2 3)

;;; e.
(defun make-graph (func start end plotting-string)
  (plot-points plotting-string (mapcar func (generate start end))))

;;; f.
(defun square (n)
  (* n n))

;(make-graph #'square -7 7 "Joshua")

;;; 9.11
(defun dot-prin1 (list-no-dots)
  (cond ((atom list-no-dots) (format t "~S" list-no-dots))
        (t (progn (format t "(")
                  (dot-prin1 (car list-no-dots))
                  (format t " . ")
                  (dot-prin1 (cdr list-no-dots))
                  (format t ")")))))
;(dot-prin1 NIL)
;(format t "~&")
;(dot-prin1 'A)
;(dot-prin1 '(A))
;(dot-prin1 '(A B))
;(dot-prin1 '(A B C))
;(dot-prin1 '(A (B) C))

;;; 9.12
;(dot-prin1 '(A . (B . C))); you get the same thing back

;;; 9.13
;'(A . B); you get a dotted pair

;;; 9.14
; It would infinitely run printing "(FOO . (FOO . " and so on without closing
; parenthesis.
; It would run infinitely too but without printing anything.

;;; 9.15
(defun hybrid-prin1 (mixed)
  (progn (format t "(~S" (car mixed))
         (hybrid-prin1-cdr (cdr mixed))))
(defun hybrid-prin1-cdr (some-cdr)
  (cond ((null some-cdr) (format t ")"))
        ((listp some-cdr) (progn (format t " ~S" (car some-cdr))
                                 (hybrid-prin1-cdr (cdr some-cdr))))
        (t (format t " . ~S)" some-cdr))))
;(hybrid-prin1 '(A . NIL))
;(format t "~&")
;(hybrid-prin1 '(A . B))
;(hybrid-prin1 '(A . (B . NIL)))
;(hybrid-prin1 '(A . (B . C)))
;(hybrid-prin1 '(A . (B . (C . D))))
;(hybrid-prin1 '((A . NIL) . (B . (C . D))))
;(hybrid-prin1 '(A B C D))
