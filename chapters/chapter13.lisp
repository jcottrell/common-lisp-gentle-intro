;;; Vectors
(setf some-array '#(things inside 1 2 3 etc))
;
;; print: global "print vector" variable *print-array*
;(setf *print-array* t); will make an array print, seems to be on my default
;
(aref some-array 1); 'inside
;
;; modify
(setf (aref some-array 5) 4); '#(thing inside 1 2 3 4)

;;; Hash tables
(setf some-hash (make-hash-table))
;
;; additions
(setf (gethash 'john some-hash) '(attorney (16 maple drive)))
;
;; reference
(gethash 'john some-hash)
; two values are returned, the value you looked up and whether the key was
; found

;;; Property lists
; Stored as a list of alternating indicators[/keys?] and values.
; Very old, better to use hashes or association lists
;
;; add / modify
(setf (get 'some-symbol 'some-indicator) 'indicator-value)
;
;; reference
(get 'some-symbol 'some-indicator); 'indicator-value
;
;; get all
(symbol-plist 'some-symbol)
;
;; remove
(remprop 'some-symbol 'some-indicator)

;; pushnew is generalized but good for property lists because it checks for
;; existance before appending a new value
(pushnew 'indicator-value (get 'some-symbol 'some-indicator))

;;; 13.1
(setf (get 'alpha 'fooprop) '(a b c d e))
(defun subprop (some-symbol value-to-remove prop-name)
  (setf (get some-symbol prop-name)
        (remove-if #'(lambda (possible-match)
                       (equalp value-to-remove possible-match))
                   (get some-symbol prop-name))))
(subprop 'alpha 'd 'fooprop); (symbol-plist 'alpha) '(fooprop (a b c e))

;;; 13.2
(defun addprop (target-symbol element-to-add prop-to-add-element-to)
  (pushnew element-to-add (get target-symbol prop-to-add-element-to)))
(defun record-meeting (left-person right-person)
  (addprop left-person right-person 'has-met)
  (addprop right-person left-person 'has-met))
;(record-meeting 'little-red 'wolf)
;(record-meeting 'wolf 'grandma)
(defun forget-meeting (left-person right-person)
  (subprop left-person right-person 'has-met)
  (subprop right-person left-person 'has-met))
;(forget-meeting 'wolf 'little-red); (symbol-plist 'wolf)

;;; 13.3
(defun find-pair (target-prop plist-list)
  (cond ((null plist-list) nil)
        ((equalp target-prop (car plist-list)) (cadr plist-list))
        (t (find-pair target-prop (cddr plist-list)))))

(defun my-get (target-symbol target-prop)
    (find-pair target-prop (symbol-plist target-symbol)))
; TODO 2025-04-26 the above needs to find the possible prop and return the next
;      value in the list of pairs
;      Example: (record-meeting 'grandma 'wolf)
;               (setf (get 'wolf 'big-ears) t)
;               (symbol-plist 'wolf); '(big-ears t has-met (grandma))
;               ; so first find 'has-met then return the next value
;(my-get 'wolf 'has-met)
;(my-get 'wolf 'big-ears)

;;; 13.4
(setf (get 'a 'foo) nil)
(setf (get 'a 'bar) t)
(symbol-plist 'a)
(defun get-next-prop (target remaining)
  (cond ((null remaining) nil)
        ((equalp target (car remaining)) t)
        (t (get-next-prop target (cddr remaining)))))
(defun hasprop-p (target-symbol target-prop)
  (get-next-prop target-prop (symbol-plist target-symbol)))
(hasprop-p 'a 'foo); T

;;; 13.5
;
;; Length is faster to find. Individual access is fast.

;;; 13.6
;
;; Very fast to add things to the front of a list. Lists can be added and
;; separated more easily.

;;; 13.7
;
;; The same number of cons cell is needed for both plist and a dotted pair.

;;; 13.8
;;;
;;; a.
(setf *hist-array* '#())
(setf *total-points* 0)
;;; b.
(defun new-histogram (bin-total)
  (setf *hist-array* (make-array bin-total :initial-element 0))
  (setf *total-points 0))
;;; c.
(defun record-value (random-value)
  (if (or (< random-value 0)
          (> random-value (length *hist-array*)))
      (progn (format t "~%Error: ~A is out of bounds")
             nil)
      (progn (setf (aref *hist-array* random-value)
                   (1+ (aref *hist-array* random-value)))
             (setf *total-points* (1+ *total-points*))
             t)))
;;; d.
(defun print-hist-line (line-number)
  (format t "~% ~2@A [~3@A] "
          line-number
          (aref *hist-array* line-number))
  (dotimes (not-used (aref *hist-array* line-number))
    (format t "*")))
;;; e.
(defun print-histogram ()
  (let ((running-total 0))
    (dotimes (current (length *hist-array*))
      (print-hist-line current)
      (setf running-total (+ running-total (aref *hist-array* current))))
    (format t "~%~8@A total" running-total)))

;;; 13.9
(setf *crypto-text* "zj ze kljjls jf slapzi ezvlij pib kl jufwxuj p hffv jupi jf enlpo pib slafml pvv bfwkj")
;
;;; a.
(setf *encipher-table* (make-hash-table :test 'equalp))
(setf *decipher-table* (make-hash-table :test 'equalp))
;
;;; b.
(defun make-substitution (from to)
  (setf (gethash from *decipher-table*) to)
  (setf (gethash to *encipher-table*) from))
;(make-substitution #\a #\b)
;(gethash #\a *decipher-table*); #\b, T
;(gethash #\b *encipher-table*); #\a, T
;(gethash #\c *decipher-table*); NIL, NIL
;
;;; c.
(defun undo-substitution (from)
  (let ((to (gethash from *decipher-table*)))
    (setf (gethash from *decipher-table*) nil)
    (setf (gethash to *encipher-table*) nil)))
;(undo-substitution #\a)
;(gethash #\a *decipher-table*); NIL, T
;(gethash #\b *encipher-table*); NIL, T
;
;;; d.
(defun clear ()
  (clrhash *decipher-table*)
  (clrhash *encipher-table*))
;(clear)
;(describe *decipher-table*)
;(describe *encipher-table*)
;;; e.
(defun decipher-string (encoded)
  (coerce (mapcar (lambda (char)
                   (or (gethash char *decipher-table*) #\Space))
               (coerce encoded 'list))
          'string))
;(make-substitution #\a #\b);
;(decipher-string "aaa")
;
;;; f.
(defun show-line (cryptogram)
  (format t "~%~A~%" cryptogram)
  (format t "~A~%" (decipher-string cryptogram)))
;(show-line "a bc def ghijk a")
;
;;; g.
; I don't see how it's different from f.
;
;;; h.
(defun get-first-character (xs)
  (char-downcase
   (char (format nil "~A" xs) 0)))
;
;;; i.
(defun read-letter ()
  (let ((instruction (read)))
    (cond ((equalp 'end instruction) 'end)
          ((equalp 'undo instruction) 'undo)
          (t (get-first-character instruction)))))
;
;;; j.
(defun sub-letter (character)
  (let ((deciphered (gethash character *decipher-table*)))
    (if deciphered
        (format t "~%The letter ~A was already deciphered to ~A~%" character deciphered)
        (progn (format t "What does ~A decipher to? " character)
               (let ((deciphered-to (read-letter)))
                 (if (and (characterp deciphered-to)
                          (not (gethash deciphered-to *encipher-table*)))
                     (make-substitution character deciphered-to)
                     (format t "~%Please enter a character that ~A deciphers to." character)))))))
;
;;; k.
(defun undo-letter ()
  (format t "~%Undo which letter? ")
  (let ((letter (read-letter)))
    (if (and (characterp letter)
             (gethash letter *decipher-table*))
        (undo-substitution letter)
        (format t "~%Could not undo ~A~%." letter))))
;
;;; l.
(defun solve (cryptogram)
  (show-line cryptogram)
  (format t "~%Substitute which letter? ")
  (let ((instruction (read-letter)))
    (if (equalp 'end instruction)
        t
        (progn (cond ((characterp instruction) (sub-letter instruction))
                     ((equalp 'undo instruction) (undo-letter))
                     (t (format t "~%Improper instruction~%")))
               (solve cryptogram)))))
