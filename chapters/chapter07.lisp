;;; 7.1
(defun add1 (initial)
  (+ 1 initial))
;(add1 5)
;(mapcar #'add1 '(1 3 5 7 9))

;;; 7.2
(setf daily-planet '((olsen jimmy 123-76-4535 cub-reporter)
                     (kent clark 089-52-6786 reporter)
                     (lane lois 951-26-1438 reporter)
                     (white perry 355-16-7439 editor)))
;(mapcar #'third daily-planet)

;;; 7.3
(mapcar #'zerop '(2 0 3 4 0 -5 -6))

;;; 7.4
(defun greater-than-5-p (some-number)
  (> some-number 5))
;(mapcar #'greater-than-5-p '(-1 6 4 10))

;;; 7.5
#'(lambda (some-number)
    (- some-number 7))

;;; 7.6
#'(lambda (something)
    (or (equal t something)
        (equal nil something)))

;;; 7.7
(defun flipper (up-down-list)
  (mapcar #'(lambda (direction)
              (cond ((equal direction 'down) 'up)
                    ((equal direction 'up) 'down)))
          up-down-list))
;(flipper '(up down up up)); '(down up down down)

;;; 7.8
(defun find-roughly-equal (to-find xs)
  (find-if #'(lambda (x)
               (and (>= x (- to-find 10))
                    (<= x (+ to-find 10))))
           xs))
;(find-roughly-equal 5 '(17 -6 11 5 6))

;;; 7.9
(defun find-nested (possibly-nested)
  (find-if #'(lambda (element)
               (and (consp element)
                    (car element)))
           possibly-nested))
;(find-nested '(1 2 3 4)); nil
;(find-nested '(1 (2) 3 (4))); '(2)
;(find-nested '(1 2 () 4 (5 6) (7))); '(5 6)

;;; 7.10
;;; a.
(setf note-table '((c . 1)
                   (c-sharp . 2)
                   (d . 3)
                   (d-sharp . 4)
                   (e . 5)
                   (f . 6)
                   (f-sharp . 7)
                   (g . 8)
                   (g-sharp . 9)
                   (a . 10)
                   (a-sharp . 11)
                   (b . 12)))

;;; b.
(defun numbers (notes)
  (mapcar #'(lambda (note)
              (cdr (assoc note note-table)))
          notes))
;(numbers '(e d c d e e e)); '(5 3 1 3 5 5 5)

;;; c.
;;; I actually wrote my note-table to be a dotted pair instead of a list so...
(defun notes (numbers)
  (mapcar #'(lambda (number)
              (car (rassoc number note-table)))
          numbers))
;(notes '(5 3 1 3 5 5 5)); '(e d c d e e e)
;
;;; but for fairness
(setf note-table-no-dots '((c 1)
                           (c-sharp  2)
                           (d 3)
                           (d-sharp 4)
                           (e 5)
                           (f 6)
                           (f-sharp 7)
                           (g 8)
                           (g-sharp 9)
                           (a 10)
                           (a-sharp 11)
                           (b 12)))
(defun notes-no-dots (numbers)
  (mapcar #'(lambda (number)
              (car (find-if #'(lambda (note-number-pair)
                               (equal number (cadr note-number-pair)))
                            note-table-no-dots)))
          numbers))
;(notes-no-dots '(5 3 1 3 5 5 5)); '(e d c d e e e)

;;; d.
;;; They both return a list of nils the same length as the input.

;;; e.
(defun raise (up-by numbers)
  (mapcar #'(lambda (n)
              (+ up-by n))
          numbers))
;(raise 5 '(5 3 1 3 5 5 5)); '(10 8 6 8 10 10 10)

;;; f.
(defun normalize (numbers)
  (mapcar #'(lambda (n)
              (cond ((> n 12) (- n 12))
                    ((< n 1) (+ n 12))
                    (t n)))
          numbers))
;(normalize '(6 10 13)); '(6 10 1)

;;; g.
(defun transpose (half-steps note-list)
  (notes (normalize (raise half-steps (numbers note-list)))))
;(transpose 5 '(e d c d e e e)); '(a g f g a a a)
;
;(transpose 11 '(e d c d e e e)); '(D-SHARP C-SHARP B C-SHARP D-SHARP D-SHARP D-SHARP)
;
;(transpose -1 '(e d c d e e e)); '(D-SHARP C-SHARP B C-SHARP D-SHARP D-SHARP D-SHARP)

;;; 7.11
(defun between-exclusive (lower-bound upper-bound)
  (lambda (some-num)
    (and (> some-num lower-bound)
         (< some-num upper-bound))))
(defun remove-between-1-and-5 (some-numbers)
  (remove-if-not (between-exclusive 1 5) some-numbers))
;(remove-between-1-and-5 '(5 3 2 -8 4 7))

;;; 7.12
(defun count-word (word sentence-list)
  (length (remove-if-not #'(lambda (some-word)
                             (equal word some-word))
                         sentence-list)))
;(count-word 'the '(the rain in spain falls mainly on the plain))

;;; 7.13
(defun sublist-2 (l-of-ls)
  (remove-if-not #'(lambda (sublist)
                     (= 2 (length sublist)))
                 l-of-ls))
;(sublist-2 '((1 2 3) (1 2) (a b c d) () (e f) ((1) (2)))); '((1 2) (e f) ((1) (2)))

;;; 7.14
(defun my-intersection (left right)
  (remove-if-not #'(lambda (r)
                     (member r left))
                 right))
;(intersection '(a b c d) '(b d)); '(b d)
;(my-intersection '(a b c d) '(b d)); '(b d)
(defun my-union (left right)
  (append left
          (remove-if #'(lambda (r)
                         (member r left))
                     right)))
;(union '(a b c d) '(b d e)); '(a b c d e)
;(my-union '(a b c d) '(b d e)); '(a b c d e)

;;; 7.15
;;; a.
(defun rank (card)
  (car card))
;(rank '(2 clubs)); 2
(defun suit (card)
  (cadr card))
;(suit '(2 clubs)); clubs
;
;;; b.
(setf my-hand '((3 hearts)
                (5 clubs)
                (2 diamonds)
                (4 diamonds)
                (ace spades)))
(defun count-suit (suit hand)
  (length (remove-if-not #'(lambda (card)
                             (equal suit (suit card)))
                         hand)))
;(count-suit 'diamonds my-hand); 2
;;; c.
(setf colors '((clubs black)
               (diamonds red)
               (spades black)
               (hearts red)))
(defun color-of (card)
  (cadr (assoc (suit card) colors)))
;(color-of '(2 clubs)); black
;(color-of '(6 hearts)); black
;
;;; d.
(defun first-red (hand)
  (find-if #'(lambda (card)
               (equal 'red (color-of card)))
           hand))
;(first-red my-hand)
;
;;; e.
(defun first-black (hand)
  (find-if #'(lambda (card)
               (equal 'black (color-of card)))
           hand))
;(first-black my-hand)
;
;;; f.
(defun what-ranks (suit hand)
  (mapcar #'rank
          (remove-if-not #'(lambda (card)
                             (equal suit (suit card)))
                         hand)))
;(what-ranks 'diamonds my-hand); '(2 4)
;(what-ranks 'spades my-hand); '(2 4)
;
;;; g.
(setf all-ranks
      '(2 3 4 5 6 7 8 9 10 jack queen king ace))
(defun higher-rank-p (left-card right-card)
  (let ((left-rank (rank left-card))
        (right-rank (rank right-card)))
    (and (not (equal left-rank right-rank))
         (member left-rank (member right-rank all-ranks))
         t)))
;(higher-rank-p '(9 hearts) '(3 hearts)); t
;(higher-rank-p '(3 hearts) '(9 hearts)); nil
;(higher-rank-p '(9 hearts) '(9 hearts)); nil
;
;;; h.
(defun high-card (hand)
  (if (> (length hand) 0)
      (high-card-so-far (cdr hand) (car hand))
      'error))
(defun high-card-so-far (remaining so-far)
  (cond ((= 0 (length remaining)) so-far)
        (t (high-card-so-far (cdr remaining)
                             (if (higher-rank-p (car remaining) so-far)
                                 (car remaining)
                                 so-far)))))
;(high-card my-hand); '(ace spades)
;(high-card '((4 hearts) (10 spades) (5 diamonds) (jack clubs) (queen spades)))
;(high-card '((4 hearts) (9 spades) (5 diamonds) (jack clubs) (10 spades)))

;;; 7.16
;(reduce #'union '((a b c) (c d a) (f b d) (g))); '(a b c d f g)

;;; 7.17
(defun lol-length (lol)
  "Calculates the total of the lists within LOL."
  (length (reduce #'append lol)))
;(lol-length '((1 2 3) (4 5 6) (7))); 7

;;; 7.18
; The #'+ and the #'* functions know they are monoids and supply the correct
; initial value in none is given, the so-called identity item/value.

;;; 7.19
(defun all-odd (numbers)
  (every #'oddp numbers))
;(all-odd '(1 3 5 7)); T
;(all-odd '(1 2 5 7)); NIL

;;; 7.20
(defun none-odd (numbers)
 (every #'(lambda (n)
            (not (oddp n)))
        numbers))
;(none-odd '(2 4 6)); T
;(none-odd '(0 0 2 4)); T

;;; 7.21
(defun not-all-odd (numbers)
  (not (all-odd numbers)))

;;; 7.22
(defun not-none-odd (numbers)
  (not (none-odd numbers)))

;;; 7.23
;all-odd and none-odd are opposites. The second could be all-even.
;The last two are the same, better named any-even.

;;; 7.29
(setf database
      '((b1 shape brick)
        (b1 color green)
        (b1 size small)
        (b1 supported-by b2)
        (b1 supported-by b3)
        (b2 shape brick)
        (b2 color red)
        (b2 size small)
        (b2 supports b1)
        (b2 left-of b3)
        (b3 shape brick)
        (b3 color red)
        (b3 size small)
        (b3 supports b1)
        (b3 right-of b3)
        (b4 shape pyramid)
        (b4 color blue)
        (b4 size large)
        (b4 supported-by b5)
        (b5 shape cube)
        (b5 color green)
        (b5 size large)
        (b5 supports b4)
        (b6 shape brick)
        (b6 color purple)
        (b6 size large)))
;;; a.
(defun match-element (left right)
  (or (equal left right)
      (equal right '?)))
;(match-element 'red 'red); T
;(match-element 'red '?); T
;(match-element 'red 'blue); NIL

;;; b.
(defun match-triple (left right)
  (every #'match-element left right))
;(match-triple '(b2 color red) '(b2 color ?)); T
;(match-triple '(b1 color red) '(b2 color green)); NIL

;;; c.
(defun fetch (pattern)
  (remove-if-not #'(lambda (db-entry)
                     (match-triple db-entry pattern))
                 database))
;(fetch '(b2 color ?)); '((b2 color red))
;(fetch '(? supports b1)); '((b2 supports b1) (b3 supports b1))

;;; d.
;(fetch '(b4 shape ?))
;(fetch '(? shape brick))
;(fetch '(b2 ? b3))
;(fetch '(? color ?))
;(fetch '(b4 ? ?))
;
;(mapcar #'third (fetch '(b4 shape ?)))
;(mapcar #'first (fetch '(? shape brick)))
;(mapcar #'second (fetch '(b2 ? b3)))
;(remove-duplicates (mapcar #'third (fetch '(? color ?))))
;(mapcar #'second (fetch '(b4 ? ?)))

;;; e.
(defun block-color-pattern (block-name)
  (list block-name 'color '?))
;(block-color-pattern 'b3)

;;; f.
(defun supporters (block-name)
  (mapcar #'third (fetch (list block-name 'supported-by '?))))
;(supporters 'b1)

;;; g.
(defun supp-cube (block-name)
  (and (find-if #'(lambda (supporter-block)
                    (fetch (list supporter-block 'shape 'cube)))
                (supporters block-name))
       t))
;(supp-cube 'b4); T
;(supp-cube 'b1); NIL

;;; h., i., j.
(defun description (block-name)
  (let* ((matches (fetch (list block-name '? '?)))
         (no-name-matches (mapcar #'cdr matches)))
    (reduce #'append
            no-name-matches)))
;(description 'b6)

;;; k.
;(description 'b1); (SHAPE BRICK COLOR GREEN SIZE SMALL SUPPORTED-BY B2 SUPPORTED-BY B3)
;(description 'b4); (SHAPE PYRAMID COLOR BLUE SIZE LARGE SUPPORTED-BY B5)

;;; l.
;(setf database (append database '((b1 material wood))))
;(setf database (append database '((b2 material plastic))))

;;; 7 Advanced Topics
;(mapcar #'(lambda (person job)
;            (list person 'gets job)
;        '(joe wilma george diane)
;        '(job1 job2 job3 job4))))
;
;;; 7.30
(setf words '((one un)
              (two deux)
              (three trois)
              (four quatre)
              (five cinq)))
(defun add-translation (dictonary additions)
  (mapcar #'(lambda (so-far addition)
              (append so-far (list addition)))
          dictonary
          additions))
;(add-translation words '(uno dos tres quatro cinco))

(setq g #'(lambda (g)
            (* g 10)))
;(funcall g 12)
