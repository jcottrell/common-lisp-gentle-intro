;;; 6.6
(defun last-element (some-list)
  (car (last some-list)))
;(last-element '(first second third))
(defun last-element-reverse (some-list)
  (car (reverse some-list)))
;(last-element-reverse '(first second third))
(defun last-element-via-nth-length (some-list)
  (nth (- (length some-list) 1) some-list))
;(last-element-via-nth-length '(first second third))

;;; 6.7
(defun next-to-last (some-list)
  (cadr (reverse some-list)))
;(next-to-last '(first second third))
(defun next-to-last-via-nth (some-list)
  (nth (- (length some-list) 2) some-list))
;(next-to-last-via-nth '(first second third))

;;; 6.8
(defun my-butlast (some-list)
  (butlast some-list))
;(my-butlast '(first second third))
(defun my-butlast-homegrown (some-list)
  (reverse (cdr (reverse some-list))))
;(my-butlast-homegrown '(first second third))
(defun my-butlast-more-growth (some-list)
  (let ((list-length (length some-list)))
    (cond ((zerop list-length) '())
          ((= 1 list-length) '())
          (t (cons (car some-list) (my-butlast-more-growth (cdr some-list)))))))
;(my-butlast-more-growth '(first second third))

;;; 6.9
;;; car

;;; 6.10
(defun palindromep (some-list)
  "True if SOME-LIST is the same forwards and backwards."
  (or (zerop (length some-list))
      (and (equal (car some-list)
                 (car (last some-list)))
          (palindromep (butlast (cdr some-list))))))
;(palindromep '(a b c d c b a))
;(palindromep '(a b c a b c))

;;; 6.11
(defun make-palindrome (some-list)
  "Makes SOME-LIST into a palindrome."
  (append some-list (reverse some-list)))
;(make-palindrome '(you and me))

;;; 6.15
(defun contains-article-p (sentence)
  (intersection '(the a an) sentence))
;(contains-article-p '(the lisper ran to the paren))
(defun contains-article-via-member-or-p (sentence)
  (or (member 'the sentence)
      (member 'a sentence)
      (member 'an sentence)))
;(contains-article-via-member-or-p '(master gave dobby a sock))
(defun contains-article-via-member-and-p (sentence)
  (not (and (member 'the sentence)
            (member 'a sentence)
            (member 'an sentence))))
;(contains-article-via-member-and-p '(lisp is like an onion))

;;; 6.18
(defun add-vowels (letters)
  (union '(a e i o u) letters))
;(add-vowels '(x a e z))

;;; 6.19
;(set-difference '() '(a b c))
;;; always returns nil
;(set-difference '(a b c) '())
;;; always returns the first
;;; 6.20 the first because the second is removed from it to get the result

;;; 6.21
(defun my-subsetp (left right)
  (not (set-difference left right)))
;(my-subsetp '(a i) '(a e i o u))
;(my-subsetp '(a x) '(a e i o u))

;;; 6.23
(length (union '(a b c) '(c d e)))

;;; 6.24
(defun set-equal (left right)
  (and (subsetp left right)
       (subsetp right left)))
;(set-equal (union '(a b c) '()) (union '() '(a b c)))

;;; 6.24
(defun proper-subsetp (left right)
  (and (subsetp left right)
       (not (set-equal left right))))
;(proper-subsetp '(a c) '(c a b))
;(proper-subsetp '(a b c) '(c a b))

;;; 6.26
;;;
;(setf e6.26-input '(large red shiny cube -vs- small shiny red four-sided pyramid))
;;;
;;; a.
(defun right-side (whole)
  (cdr (member '-vs- whole)))
;(right-side e6.26-input)
;
;;; b.
(defun left-side (whole)
  (reverse (right-side (reverse whole))))
;(left-side e6.26-input)
;
;;; c.
(defun count-common (whole)
  (length (intersection (left-side whole) (right-side whole))))
;(count-common e6.26-input)
;
;;; d.
(defun compare (vs-list)
  (list (count-common vs-list) 'common 'features))
;(compare e6.26-input)
;
;(compare '(small red metal cube -vs- red plastic small cube))

;;; implementing assoc
(defun my-assoc (search-key table)
  (let ((entry (car table)))
    (cond ((not entry) '())
          ((equalp search-key (car entry)) entry)
          (t (my-assoc search-key (cdr table))))))
;(my-assoc 'one '((one . uno) (two . dos) (three . tres) (four . quatro)))

;;; 6.28
(setf produce
      '((apple . fruit)
        (celery . veggie)
        (banana . fruit)
        (lettuce . veggie)))
(assoc 'banana produce); '(banana . fruit)
(rassoc 'fruit produce); '(apple . fruit)
(assoc 'lettuce produce); '(lettuce . veggie)
(rassoc 'veggie produce); '(celery . veggie)

;;; Example in 6.9
(setf things '((object1 large green shiny cube)
               (object2 small red dull metal cube)
               (object3 red small dull plastic cube)
               (object4 small dull blue metal cube)
               (object5 small shiny red four-sided pyramid)
               (object6 large shiny green sphere)))
(defun description (object)
  (rest (assoc object things)))
;(description '(objectx short round indiana jones character))
;(description (assoc 'object3 things))
(defun differences (left right)
  (set-exclusive-or (description left) (description right)))
;(differences 'object2 'object3)
(setf quality-table
      '((large . size)
        (small . size)
        (red . color)
        (green . color)
        (blue . color)
        (shiny . luster)
        (dull . luster)
        (metal . material)
        (plastic . material)
        (cube . shape)
        (sphere . shape)
        (pyramid . shape)
        (four-sided . shape)))
(defun quality (quality)
  (cdr (assoc quality quality-table)))
;(quality 'red)
(defun quality-difference (left right)
  (quality (first (differences left right))))
;(quality-difference 'object1 'object3)
;(quality-difference 'object1 'object6)
;(differences 'object3 'object4)
(defun contrast (left right)
  (remove-duplicates
   (sublis quality-table (differences left right))))
;(contrast 'object3 'object4)

;;; 6.29 (length things)
;;; 6.30
(setf books '((war-and-peace leo-tolstoy)
              (1984 george-orwell)
              (brave-new-world aldus-huxley)
              (the-hiding-place cory-tenboom)
              (hamlet william-shakespeare)))
;;; 6.31
(defun who-wrote (title)
  (cadr (assoc title books)))
;(who-wrote 'brave-new-world)
;;; 6.32
;;; Nothing
;;; 6.33
;;; We'd have to switch the order to author, title
;;; 6.34
(setf atlas1 '((pennsylvania (pittsburgh johnstown))
               (new-jersey (newark princeton trenton))
               (ohio (columbus))))
;(assoc 'new-jersey atlas1)
(setf atlas2 '((pittsburgh . pennsylvania)
               (newark . new-jersey)
               (johnstown . pennsylvania)
               (columbus . ohio)
               (princeton . new-jersey)
               (trenton . new-jersey)))
(defun multi-rassoc (value table)
  (multi-rassoc-recur value table '()))
(defun multi-rassoc-recur (value remaining items-so-far)
  (cond ((= 0 (length remaining)) items-so-far)
        ((equalp value (cdr (car remaining))) (multi-rassoc-recur value (cdr remaining) (append items-so-far (list (car remaining)))))
        (t (multi-rassoc-recur value (cdr remaining) items-so-far))))
;(multi-rassoc 'new-jersey atlas2)
;;; or the same thing with assoc
;;; 6.35
;;; a.
(setf nerd-states1
      (let* ((ordered-states '(sleeping eating waiting-for-a-computer programming debugging))
             (initial-state (car ordered-states))
             (terminal-state (car (last ordered-states)))
             (current-state initial-state))
        (lambda ()
          (let ((state-to-return current-state))
            (progn (setf current-state (if (equalp current-state terminal-state)
                                           initial-state
                                           (cadr (member current-state ordered-states))))
                   state-to-return)))))
;(funcall nerd-states1)
(setf nerd-states2 '(sleeping eating waiting-for-a-computer programming debugging))
;;; b.
(defun nerdus1 ()
  (funcall nerd-states1))
;(nerdus1)
(defun nerdus2 (previous-state)
  (if (equalp (car (last nerd-states2)) previous-state)
      (car nerd-states2)
      (cadr (member previous-state nerd-states2))))
;(nerdus2 'sleeping)
;(nerdus2 'eating)
;(nerdus2 'waiting-for-a-computer)
;(nerdus2 'programming)
;(nerdus2 'debugging)
;;; c.
;(nerdus2 'playing-guitar); => NIL
;;; d.
(defun sleepless-nerd (previous-state)
  (if (equalp (car (last nerd-states2)) previous-state)
      (cadr nerd-states2)
      (cadr (member previous-state nerd-states2))))
;(sleepless-nerd 'sleeping)
;(sleepless-nerd 'eating)
;(sleepless-nerd 'waiting-for-a-computer)
;(sleepless-nerd 'programming)
;(sleepless-nerd 'debugging)
;;; e.
(defun nerd-on-caffeine (previous-state)
  (cond
    ((equalp (car (last nerd-states2)) previous-state) (cadr nerd-states2))
    ((equalp (car (last nerd-states2 2)) previous-state) (car nerd-states2))
    (t (caddr (member previous-state nerd-states2)))))
;(nerd-on-caffeine 'sleeping); waiting-for-a-computer
;(nerd-on-caffeine 'eating); programming
;(nerd-on-caffeine 'waiting-for-a-computer); debugging
;(nerd-on-caffeine 'programming); sleeping
;(nerd-on-caffeine 'debugging); eating
;;; f. "It" would go _through_ 2 (i.e. after programming then sleeping, waiting to debugging)

;;; 6.35
;;; if I chose a table, as maybe I should have according
;;; to e. "Your function should use the same table as NERDUS"
;;; a-table.
(setf nerd-states3 '((sleeping . eating)
                     (eating . waiting-for-a-computer)
                     (waiting-for-a-computer . programming)
                     (programming . debugging)
                     (debugging . sleeping)))
;;; b-table.
(defun nerdus3 (previous-state)
  (cdr (assoc previous-state nerd-states3)))
;(nerdus3 'sleeping)
;(nerdus3 'eating)
;(nerdus3 'waiting-for-a-computer)
;(nerdus3 'programming)
;(nerdus3 'debugging)
;;; c-table. (nerdus3 'playing-guitar); NIL
;;; d-table.
(defun sleepless-nerd2 (previous-state)
  (let ((potentially-next (nerdus3 previous-state)))
    (if (equalp potentially-next 'sleeping)
        (nerdus3 potentially-next)
        potentially-next)))
;(sleepless-nerd2 'sleeping); eating
;(sleepless-nerd2 'eating); waiting-for-a-computer
;(sleepless-nerd2 'waiting-for-a-computer); programming
;(sleepless-nerd2 'programming); debugging
;(sleepless-nerd2 'debugging); eating
;;; e-table.
(defun nerd-on-caffeine2 (previous-state)
  (nerdus3 (nerdus3 previous-state)))
;(nerd-on-caffeine2 'sleeping); waiting-for-a-computer
;(nerd-on-caffeine2 'eating); programming
;(nerd-on-caffeine2 'waiting-for-a-computer); debugging
;(nerd-on-caffeine2 'programming); sleeping
;(nerd-on-caffeine2 'debugging); eating

;;; 6.36
(defun swap-first-last (some-list)
  (let ((list-length (length some-list)))
    (cond ((or (= 0 list-length)
               (= 1 list-length)) some-list)
          (t (append (list (car (last some-list)))
                     (reverse (cdr (reverse (cdr some-list))))
                     (list (car some-list)))))))
;(swap-first-last '(you cant buy love)); '(love cant buy you)

;;; 6.37
(defun rotate-left (some-list)
  (append (cdr some-list) (list (car some-list))))
;(rotate-left '(a b c d)); (b c d a)
(defun rotate-right (some-list)
  (append (last some-list)
          (reverse (cdr (reverse some-list)))))
;(rotate-right '(a b c d)); '(d a b c)

;;; 6.38
;(set-difference '(a b c) '(c a b))
; (1) empty list '() and (2) ... '(a b c) with '(c a b) ?

;;; 6.39
;cons

;;; 6.40
(setf member-table '((a b c d)
                     (b c d)
                     (c d)
                     (d)))
;(member 'd '(d))
;(assoc 'd member-table)

;;; 6.41
;;; Library                  upstairs-bedroom
;;; |                          |
;;; |                          |
;;; |                        front-stairs
;;; |                             |
;;; back-stairs                   |
;;; |                             |
;;; |                             |
;;; |                        living-room ----- kitchen
;;; |                             |              |
;;; |                             |              |
;;; |                             |              |
;;; downstairs-bedroom ----- dining-room ----- pantry
(setf rooms
      '((living-room (north front-stairs)
         (south dining-room)
         (east kitchen))
        (upstairs-bedroom (west library)
         (south front-stairs))
        (dining-room (north living-room)
         (east pantry)
         (west downstairs-bedroom))
        (kitchen (west living-room)
         (south pantry))
        (pantry (north kitchen)
         (west dining-room))
        (downstairs-bedroom (north back-stairs)
         (east dining-room))
        (back-stairs (south downstairs-bedroom)
         (north library))
        (front-stairs (north upstairs-bedroom)
         (south living-room))
        (library (east upstairs-bedroom)
         (south back-stairs))))
;;; a.
(defun choices (room-name)
  (cdr (assoc room-name rooms)))
;(choices 'pantry)
;;; b.
(defun look (direction location)
  (car (cdr (assoc direction (choices location)))))
;(look 'north 'pantry); kitchen
;;; c.
(setf loc 'pantry)
(defun set-robbie-location (place)
  "Moves Robbie to PLACE by setting
   the variable LOC."
  (setf loc place))
;(set-robbie-location 'pantry)
;(set-robbie-location 'front-stairs)
;;; d.
(defun how-many-choices ()
  (length (choices loc)))
;(how-many-choices)
;;; e.
(defun upstairsp (place)
  (if (member place '(library upstairs-bedroom)) t))
;(upstairsp 'library)
;(upstairsp 'living-room)
(defun onstairsp (place)
  (if (member place '(front-stairs back-stairs)) t))
;(onstairsp 'front-stairs)
;(onstairsp 'library)
;;; f.
(defun where ()
  (append (list 'robbie 'is)
          (cond ((onstairsp loc) (list 'on 'the loc))
                (t (cons (if (upstairsp loc) 'upstairs 'downstairs)
                         (list 'in 'the loc))))))
;(where)
;;; g.
(defun move (direction)
  (let ((next-place (look direction loc)))
    (cond (next-place (set-robbie-location next-place)
                      (where))
          (t '(ouch! robbie hit a wall)))))
;(move 'north)
;(move 'south)
;;; h.
;(set-robbie-location 'pantry)
;
;; pantry to the library
;(move 'west)
;(move 'west)
;(move 'north)
;(move 'north)
;
;;library to the kitchen
;(move 'east)
;(move 'south)
;(move 'south)
;(move 'east)

;;; 6.42
(defun royal-we (i-list)
  (subst 'we 'i i-list))
;(royal-we '(if i learn lisp i will be pleased))
