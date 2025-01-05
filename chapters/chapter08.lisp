;;; 8.1
;;; The (oddp (first x)) is never true.

;;; 8.2
(defun anyoddp-if (ls)
  (if (not (null ls))
      (or (oddp (car ls))
          (anyoddp-if (rest ls)))))
;(anyoddp-if '(3142 5798 6550 8914)); NIL
;(anyoddp-if '(2 4 6 7 9 11)); T

;;; 8.3
(defun fact (n)
  (cond ((zerop n) 1)
        (t (* n (fact (- n 1))))))
;(fact 5); 120
;(fact 20); 2432902008176640000
;(fact 20.0); 2.432902e18
;(trace fact)
(defun my-fact (current)
  (my-fact-iter current '()))
(defun my-fact-iter (current so-far)
  (cond ((zerop current) (apply #'* so-far))
        (t (my-fact-iter (- current 1) (cons current so-far)))))
;(my-fact 5); 120
;(trace my-fact my-fact-iter)
(defun fact-internal-recur (target &optional (start-list '(1)))
  (cond ((= 1 target) (apply #'* start-list))
        (t (fact-internal-recur (- target 1) (cons target start-list)))))
;(fact-internal-recur 5); 120
;(trace fact-internal-recur)
;(fact-internal-recur 5)

;;; 8.4
(defun laugh (ha-number)
  (if (zerop ha-number)
      '()
      (cons 'ha (laugh (- ha-number 1)))))
;(laugh 3)

;;; 8.5
(defun add-up (numbers)
  (cond ((null numbers) 0)
        (t (+ (car numbers)
              (add-up (cdr numbers))))))
;(add-up '(2 3 7)); 12

;;; 8.6
(defun alloddp-bool (possibles)
  (or (null possibles)
      (and (oddp (car possibles))
           (alloddp (cdr possibles)))))
;(alloddp-bool '(1 3 5 7)); T
(defun alloddp (possibles)
  (cond ((null possibles) t)
        (t (and (oddp (car possibles))
                (alloddp (cdr possibles))))))
;(alloddp '(1 3 5 7 9)); T
;(alloddp '(1 3 5 2 9)); NIL

;;; 8.7
(defun rec-member (search-thing possible-things)
  (cond ((null possible-things) '())
        ((equal search-thing (car possible-things)) possible-things)
        (t (rec-member search-thing (cdr possible-things)))))
;(rec-member 'e '(a b c d e f)); '(e f)
;(rec-member 'e '(a b c)); '()

;;; 8.8
(defun rec-assoc (item alist)
  (cond ((null alist) '())
        ((equal item (car (car alist))) (car alist))
        (t (rec-assoc item (cdr alist)))))
;(assoc 'person2 '((person1 joe) (person2 jan) (person3 jay)))
;(rec-assoc 'person2 '((person1 joe) (person2 jan) (person3 jay)))

;;; 8.9
(defun rec-nth (place things)
  (cond ((null things) '())
        ((zerop place) (car things))
        (t (rec-nth (- place 1) (cdr things)))))
;(nth 3 '(1 2 3 4 5)); 4
;(rec-nth 3 '(1 2 3 4 5)); 4

;;; 8.10
(defun add1 (n)
  (+ 1 n))
(defun sub1 (n)
  (- n 1))
(defun rec-plus (x y)
  (cond ((zerop y) x)
        (t (rec-plus (add1 x) (sub1 y)))))
;(rec-plus 3 2); 5

;;; 8.11
(defun fib (n)
  (cond ((or (zerop n)
             (= 1 n))
         1)
        (t (+ (fib (- n 1))
              (fib (- n 2))))))
;(fib 5); 8

;;; 8.12
;correctly: '(0 7)
;Incorrectly: '()

;;; 8.13
;You could give a negative number

;;; 8.14
;(defun too-much () (too-much))

;;; 8.15
;The car is x and the cdr is the rest of the list 🙂
;The count-slices function will run infinitely.

;;; 8.16
;We could get an error when oddp tested an empty list.

;;; 8.17
(defun find-first-odd (things)
  (cond ((null things) '())
        ((oddp (car things)) (car things))
        (t (find-first-odd (cdr things)))))
;(find-first-odd '()); NIL
;(find-first-odd '(2 4 6 8 10)); NIL
;(find-first-odd '(2 4 6 7 8 10)); 7

;;; 8.18
(defun last-element (things)
  (cond ((null (cdr things)) (car things))
        (t (last-element (cdr things)))))
;(last-element '()); NIL
;(last-element '(1 3 4 2)); 2

;;; 8.19
;The anyoddp function would still work for lists with an odd number in them.
;It would result in an infinite loop for lists with no odd numbers.

;;; 8.20
;The fact function would be single recursion.
;Function:  fact
;End test:  (zerop n)
;End-value: 1
;Aug fun:   *
;Aug value: n
;Reduced-x: (- n 1)

;;; 8.21
(defun add-nums (n)
  (cond ((zerop n) 0)
        (t (+ n (add-nums (sub1 n))))))
;(add-nums 5); 15

;;; 8.22
(defun all-equal (things)
  (cond ((< (length things) 2) t)
        (t (and (equal (car things) (cadr things))
                (all-equal (cdr things))))))
;(all-equal '(1 1 1 1)); T
;(all-equal '(i i i i)); T
;(all-equal '(i i e i)); NIL

;;; 8.23
;| N   |  First to cons | Second to cons
;---------------------------------------
;| 5   |  'ha           | (laugh 4)
;| 4   |  'ha           | (laugh 3)
;| 3   |  'ha           | (laugh 2)
;| 2   |  'ha           | (laugh 1)
;| 1   |  'ha           | (laugh 0)
;| 0   |  'ha           | '()

;;; 8.24
(defun count-down (high)
  (cond ((<= high 0) '())
        (t (cons high (count-down (- high 1))))))
;(count-down 5); '(5 4 3 2 1)

;;; 8.25
(defun fact-down (high)
  (apply #'* (count-down high)))
;(fact-down 5); 120

;;; 8.26
(defun count-down1 (high)
  (cond ((zerop high) '(0))
        (t (cons high (count-down1 (- high 1))))))
;(count-down1 5); '(5 4 3 2 1 0)
(defun count-down2 (high)
  (cond ((< high 0) '())
        (t (cons high (count-down2 (- high 1))))))
;(count-down2 5); '(5 4 3 2 1 0)

;;; 8.27
(defun square-list (simples)
  (cond ((null simples) '())
        (t (cons (* (car simples) (car simples))
                 (square-list (cdr simples))))))
;(square-list '(3 4 5 6)); '(9 16 25 36)

;;; 8.28
(defun my-nth-shorter (search-index things)
  (cond ((null things) '())
        ((> search-index (- (length things) 1)) '())
        ((zerop search-index) (car things))
        (t (my-nth-shorter (- search-index 1) (cdr things)))))
;(my-nth-shorter 2 '(a e i o u)); 'i
;(my-nth-shorter 10000 '(a e i o u)); '()

;;; 8.29
; That seems to be what I did with rec-member for 8.7

;;; 8.30
; That seems to be what I did with rec-assoc for 8.8

;;; 8.31
(defun compare-lengths (left right)
  (cond ((and (null left)
              (null right)) 'same-length)
        ((null left) 'second-is-longer)
        ((null right) 'first-is-longer)
        (t (compare-lengths (cdr left) (cdr right)))))

;;; 8.32
(defun sum-numeric-elements (elements &optional (total 0))
  (cond ((null elements) total)
        ((numberp (car elements)) (sum-numeric-elements (cdr elements) (+ total (car elements))))
        (t (sum-numeric-elements (cdr elements) total))))
;(sum-numeric-elements '(3 bears 3 bowls 1 girl)); 7
;(trace sum-numeric-elements)

;;; 8.33
(defun my-remove (item-to-remove items)
  (cond ((null items) '())
        ((equal item-to-remove (car items)) (my-remove item-to-remove (cdr items)))
        (t (cons (car items) (my-remove item-to-remove (cdr items))))))
;(remove 'sinning '(my sinning heart))
;(my-remove 'sinning '(my sinning heart)); '(my heart)

;;; 8.34
(defun my-intersection (left right)
  (cond ((null left) '())
        ((find (car left) right) (cons (car left) (my-intersection (cdr left) right)))
        (t (my-intersection (cdr left) right))))
;(my-intersection '(one two three) '(four two five six seven)); '(two)

;;; 8.35
(defun my-set-difference (left right)
  (cond ((null left) '())
        ((find (car left) right) (my-set-difference (cdr left) right))
        (t (cons (car left) (my-set-difference (cdr left) right)))))
;(my-set-difference '(one two three four) '(three two)); '(one four)

;;; 8.36
(defun count-odd1 (numbers)
  (cond ((null numbers) 0)
        ((oddp (car numbers)) (+ 1 (count-odd1 (cdr numbers))))
        (t (count-odd1 (cdr numbers)))))
;(count-odd1 '(4 5 6 7 8)); 2
(defun count-odd2 (numbers)
  (cond ((null numbers) 0)
        (t (+ (if (oddp (car numbers)) 1 0)
              (count-odd2 (cdr numbers))))))
; I didn't understand "you will need to write a conditional expression for the
; augmentation value" except to do it like this
;(count-odd2 '(4 5 6 7 8)); 2

;;; 8.37
(defun combine (number1 number2)
  (+ number1 number2))
(defun fib-combine (n)
  (cond ((<= n 1) 1)
        (t (combine (fib-combine (- n 1))
                    (fib-combine (- n 2))))))
;(trace combine fib-combine)
;(fib-combine 4)

;;; 8.38
;;; The list would be all dotted pairs of q.

;;; 8.39
(defun count-atoms (bush)
  (cond ((null bush) 1)
        ((atom (car bush)) (+ 1 (count-atoms (cdr bush))))
        (t (+ (count-atoms (car bush))
              (count-atoms (cdr bush))))))
;(count-atoms '(a (b) c))
;(count-atoms '(a b))

;;; 8.40
(defun count-cons (bush)
  (cond ((null bush) 0)
        ((atom bush) 0)
        ((consp bush)
         (+ 1
            (count-cons (car bush))
            (count-cons (cdr bush))))
        (t 0)))
;(count-cons '(foo)); 1
;(count-cons '(foo bar)); 2
;(count-cons '((foo))); 2
;(count-cons 'fred); 0

;;; 8.41
(defun sum-tree (tree)
  (cond ((null tree) 0)
        ((atom tree) (if (numberp tree) tree 0))
        (t (+ (sum-tree (car tree))
              (sum-tree (cdr tree))))))
;(sum-tree '((3 bears) (3 bowls) (1 girl))); 7

;;; 8.41
(defun my-subst (new old tree)
  (cond ((null tree) '())
        ((atom tree) (if (equal tree old) new tree))
        (t (append (list (my-subst new old (car tree)))
                   (my-subst new old (cdr tree))))))
;(my-subst 'new 'old '(some old things should not die))
;(my-subst 'new 'old '(some (old things) should (not) die))
;(my-subst 'new 'old '(some ((old) things) are ((not) (buried (old)))))

;;; 8.43
(defun flatten (tree &optional (flat '()))
  (cond ((null tree) flat)
        ((atom tree) (append flat (list tree)))
        (t (append (flatten (car tree))
                   (flatten (cdr tree))))))
;(flatten '((a b (r)) a c (a d ((a (b)) r) a)))

;;; 8.44
(defun tree-depth (tree &optional (total 0))
  (cond ((null tree) total)
        ((atom tree) total)
        (t (max (tree-depth (car tree) (+ 1 total))
                (tree-depth (cdr tree) (+ 1 total))))))
;(tree-depth '(a . b)); 1
;(tree-depth '((a b c d))); 5
;(tree-depth '((a . b) . (c . d))); 2

;;; 8.45
(defun paren-depth (tree &optional (total 0))
  (cond ((null tree) total)
        ((atom tree) total)
        (t (max (paren-depth (car tree) (+ 1 total))
                (paren-depth (cdr tree) total)))))
;(paren-depth '(a b c)); 1
;(paren-depth '(a b ((c) d) e)); 3

;;; 8.46
(defun count-up (target)
  (cond ((= 0 target) '())
        (t (append (count-up (- target 1)) (list target)))))
;(count-up 5); '(1 2 3 4 5)

;;; 8.47
(defun make-loaf (size)
  (if (= size 0)
      '()
      (cons 'x (make-loaf (- size 1)))))
;(make-loaf 4); '(x x x x)

;;; 8.48
(defun bury (what how-deep)
  (cond ((= 0 how-deep) '())
        ((= 1 how-deep) (list what))
        (t (list (bury what (- how-deep 1))))))
;(bury 'fred 2); ((fred))
;(bury 'fred 5); (((((fred)))))

;;; 8.49
(defun pairings (left right)
  (mapcar (lambda (l r) (list l r)) left right))
;(pairings '(a b c) '(1 2 3)); '((a 1) (b 2) (c 3))

;;; 8.50
(defun sublists (list-of-lists)
  (cond ((null list-of-lists) '())
        (t (cons list-of-lists
                 (sublists (rest list-of-lists))))))
;(sublists '(fee fie foe)); '((fee fie foo) (fie foe) (foe))

;;; 8.51
(defun my-reverse1 (things)
  (cond ((null things) '())
        (t (append (my-reverse1 (cdr things))
                   (list (car things))))))
;(my-reverse1 '(a b c)); '(c b a)
(defun my-reverse2 (things)
  (rev-recur things '()))
(defun rev-recur (remaining result)
  (cond ((null remaining) result)
        (t (rev-recur (cdr remaining)
                      (cons (car remaining)
                            result)))))
;(my-reverse2 '(a b c)); '(c b a)
(defun my-reverse3 (things &optional (result '()))
  (cond ((null things) result)
        (t (my-reverse3 (cdr things)
                        (cons (car things)
                              result)))))
;(my-reverse3 '(a b c)); '(c b a)

(defun my-union (left right &optional (result '()))
  (cond ((null left) (append result right))
        ((member (car left) right) (my-union (cdr left) right result))
        (t (my-union (cdr left) right (cons (car left) result)))))
;(my-union '(a b c d e) '(g f e c)); '(a b c d e f g)

;;; 8.53
(defun largest-even (some-numbers)
  (apply #'max (remove-if-not #'evenp some-numbers)))
;(largest-even '(5 2 4 3)); 4
(defun largest-even-rec (some-numbers &optional (largest-so-far 0))
  (cond ((null some-numbers) largest-so-far)
        ((and (evenp (car some-numbers))
              (> (car some-numbers)
                 largest-so-far))
         (largest-even-rec (cdr some-numbers) (car some-numbers)))
        (t (largest-even-rec (cdr some-numbers) largest-so-far))))
;(largest-even-rec '(5 2 4 3)); 4

;;; 8.54
(defun huge (base-and-power)
  (expt base-and-power base-and-power))
;(huge 2); 4
;(huge 3); 27
;(huge 4); 256

;;; 8.55
;A recursive function calls itself.

;;; 8.56
(defun every-other (things &optional (take t) (result '()))
  (cond ((null things) result)
        (take (every-other (cdr things) nil (append result (list (car things)))))
        (t (every-other (cdr things) t result))))
;(every-other '(a b c d e f g)); '(a c e g)

;;; 8.57
(defun left-half (things)
  (left-half-rec things '()))
(defun left-half-rec (remaining result)
  (cond ((or (null remaining)
             (>= (length result)
                 (length remaining)))
         result)
        (t (left-half-rec (cdr remaining)
                          (append result
                                  (list (car remaining)))))))
;(left-half '(a b c d e)); '(a b c)
;(left-half '(1 2 3 4 5 6 7 8)); '(1 2 3 4)

;;; 8.58
(defun merge-lists (left right)
  (cond ((null left) right)
        ((null right) left)
        ((> (car left) (car right)) (append (list (car right))
                                            (merge-lists left (cdr right))))
        (t (append (list (car left))
                   (merge-lists (cdr left) right)))))
;(merge-lists '(1 2 6 8 10 12) '(2 3 5 9 13)); '(1 2 2 3 5 6 8 9 10 12 13)

;;; 8.59
(fact 5); 120
;(= (fact 5) (/ (fact 6) 6)); T
(defun broken-fact (n)
  (cond ((zerop n) 1)
        (t (/ (fact (+ n 1))
              (+ n 1)))))
;(broken-fact 5)
;It will fail if implemented by calling broken-fact instead of fact.
;The recursion is broken because the terminating clause won't be reached for
;anything beyond 0.

;;; Lisp Toolkit: The Debugger
(defun debug-fact (n)
  (cond ((zerop n) (progn (break "N is zero")
                          0))
        (t (* n (debug-fact (- n 1))))))

;;;
;;; Keyboard Exercise
;;;
(setf family
      '((colin nil nil)
        (deirdre nil nil)
        (arthur nil nil)
        (kate nil nil)
        (frank nil nil)
        (linda nil nil)
        (suzanne colin deirdre)
        (bruce arthur kate)
        (charles arthur kate)
        (david arthur kate)
        (ellen arthur kate)
        (george frank linda)
        (hillary frank linda)
        (andre nil nil)
        (tamara bruce suzanne)
        (vincent bruce suzanne)
        (wanda nil nil)
        (ivan george ellen)
        (julie george ellen)
        (marie george ellen)
        (nigel andre hillary)
        (frederick nil tamara)
        (zelda vincent wanda)
        (joshua ivan wanda)
        (quentin nil nil)
        (robert quentin julie)
        (olivia nigel marie)
        (peter nigel marie)
        (erica nil nil)
        (yvette robert zelda)
        (diane peter erica)))

;;; 8.60
(defun find-family-member (name)
  (assoc name family))
(defun name-of (family-member)
  (first family-member))
(defun father-of (family-member)
  (second family-member))
(defun mother-of (family-member)
  (third family-member))
;;; a.
(defun father (name)
  (father-of (find-family-member name)))
(defun mother (name)
  (mother-of (find-family-member name)))
(defun parents (name)
  (let ((child (find-family-member name)))
    (flatten (list (father-of child) (mother-of child)))))
(defun children (name)
  (if name
      (mapcar #'name-of
          (remove-if-not (lambda (family-member)
                            (or (equalp name (father-of family-member))
                                (equalp name (mother-of family-member))))
                         family))))
;(father 'suzanne); colin
;(mother 'zelda); wanda
;(parents 'suzanne); '(colin deirdre)
;(parents 'frederick); '(tamera)
;(children 'arthur); '(bruce charles david ellen)
;(father nil); nil
;(mother nil); nil
;(parents nil); nil
;(children nil); nil
;
;;; b.
(defun siblings (name)
  (let* ((family-member (find-family-member name))
         (mom (mother-of family-member))
         (dad (father-of family-member)))
    (remove-if (lambda (childs-name)
                      (equalp name childs-name))
                   (union (children mom)
                          (children dad)))))
;(siblings 'bruce)
;(siblings 'zelda)

;;; c.
(defun mapunion (fun things)
  (reduce #'union (mapcar fun things)))
;(mapunion #'rest '((1 a b c) (2 e c j) (3 f a b c d)))

;;; d.
(defun grandparents (name)
  (mapunion #'parents (parents name)))
;(mapunion #'parents (parents 'vincent))

;;; e.
(defun cousins (name)
  (let* ((family-member (find-family-member name))
         (mom (mother-of family-member))
         (dad (father-of family-member)))
    (mapunion #'children
         (append (siblings mom)
                 (siblings dad)))))
;(cousins 'julie)

;;; f.
(defun descended-from (possible-descendant possible-ancestor)
  (let ((family-member (find-family-member possible-descendant)))
    (and (not (null possible-descendant))
         (or (equalp (mother-of family-member) possible-ancestor)
             (equalp (father-of family-member) possible-ancestor)
             (descended-from (mother family-member) possible-ancestor)
             (descended-from (father possible-descendant) possible-ancestor)))))
;(descended-from 'tamara 'arthur)
;(descended-from 'tamara 'linda)

;;; g.
(defun ancestors (name)
  (let* ((family-member (find-family-member name))
         (mom (mother-of family-member))
         (dad (father-of family-member)))
    (cond ((null family-member) '())
          (t (append (if mom (list mom))
                     (if dad (list dad))
                     (if mom (ancestors mom))
                     (if dad (ancestors dad)))))))
;(ancestors 'marie)

;;; h.
(defun generation-gap (younger older &optional (gap 1))
  (let* ((family-member (find-family-member younger))
         (mom (mother-of family-member))
         (dad (father-of family-member)))
    (cond ((null family-member) 0)
          ((or (equalp mom older)
               (equalp dad older)) gap)
          (t (max (generation-gap mom older (+ 1 gap))
                  (generation-gap dad older (+ 1 gap)))))))
;(generation-gap 'suzanne 'colin)
;(generation-gap 'frederick 'colin)
;(generation-gap 'frederick 'linda)
;(generation-gap 'yvette 'frank)

;;; i
; 1. (descended-from 'robert 'deirdre); NIL
; 2. (ancestors 'yvette); '(zelda robert wanda vincent suzanne bruce deirdre colin
;     kate arthur julie quentin ellen george kate arthur linda frank)
; 3. (generation-gap 'olivia 'frank); 3
; 4. (cousins 'peter); '(joshua robert)
; 5. (grandparents 'olivia); '(hillary andre george ellen)

;;; 8.61
(defun rec-count-up (target &optional (result '()))
  (cond ((zerop target) result)
        (t (rec-count-up (- target 1) (cons target result)))))
;(rec-count-up 5)

;;; 8.62
(defun rec-fact (n &optional (result 1))
  (cond ((zerop n) result)
        (t (rec-fact (- n 1) (* n result)))))
;(rec-fact 5); 120

;;; 8.63
;see my-union from 8.51
;
(defun tail-rec-intersection (left right &optional (result '()))
  (cond ((null left) result)
        ((find (car left) right) (tail-rec-intersection (cdr left) right (cons (car left) result)))
        (t (tail-rec-intersection (cdr left) right result))))
;(tail-rec-intersection '(one two three) '(four two five six seven)); '(two)
;
(defun tail-rec-set-difference (left right &optional (result '()))
  (cond ((null left) result)
        ((find (car left) right) (tail-rec-set-difference (cdr left) right result))
        (t (tail-rec-set-difference (cdr left) right (append result (list (car left)))))))
;(tail-rec-set-difference '(one two three four) '(three two)); '(one four)

;;; 8.64
(defun tree-find-if (fun haystack)
  (cond ((null haystack) '())
        ((atom haystack) (if (funcall fun haystack) haystack nil))
        (t (or (tree-find-if fun (car haystack))
               (tree-find-if fun (cdr haystack))))))
;(tree-find-if #'oddp '((2 4) (5 6) (7))); 5

;;; 8.65
(defun tr-count-slices (loaf)
  (labels ((keep-counting (remaining total-so-far)
             (cond ((null remaining) total-so-far)
                   (t (keep-counting (cdr remaining) (+ 1 total-so-far))))))
    (keep-counting loaf 0)))
;(tr-count-slices '(x x x x x))
;
(defun tr-reverse (original-list)
  (labels ((reverse-more (remaining reversed)
             (cond ((null remaining) reversed)
                   (t (reverse-more (cdr remaining) (cons (car remaining) reversed))))))
    (reverse-more original-list '())))
;(tr-reverse '(e d c b a)); '(a b c d e)

;;; 8.66
(defun arith-eval (expression)
  (cond ((null expression) nil)
        ((atom expression) expression)
        (t (let ((left (first expression))
                 (operator (second expression))
                 (right (third expression)))
             (cond ((equalp operator '+) (+ (arith-eval left)
                                            (arith-eval right)))
                   ((equalp operator '-) (- (arith-eval left)
                                            (arith-eval right)))
                   ((equalp operator '*) (* (arith-eval left)
                                            (arith-eval right)))
                   ((equalp operator '/) (/ (arith-eval left)
                                            (arith-eval right)))
                   (t nil))))))
;(arith-eval '(2 + (3 * 4))); 14

;;; 8.67
(defun legalp (expression)
  (and (arith-eval expression)
       t))
;(arith-eval nil); NIL
;(arith-eval '(a b c d)); NIL
;(legalp nil); NIL
;(legalp 4); T
;(legalp '(4 + 5)); T
;(legalp '(a b c d)); NIL

;;; 8.68
; For a proper list (non-dotted list) definition:
; "NIL is a proper list, and so is any cons cell whose cdr is also a proper
; list."

;;; 8.69
; All positive numbers are either prime or composite numbers. A prime number
; can be divided only by itself and one. A composite number can be factored
; into prime numbers.

;;; 8.70
(defun factors (n)
  (factors-help n 2))
(defun factors-help (n p)
  (cond ((equal n 1) nil)
        ((zerop (rem n p))
         (cons p (factors-help (/ n p) p)))
        (t (factors-help n (+ p 1)))))
;(factors 60)
(defun factor-tree (n &optional (p 2))
  (cond ((equalp n 1) '())
        ((equalp n p) n)
        ((zerop (rem n p))
         (list n p (factor-tree (/ n p) p)))
        (t (factor-tree n (+ p 1)))))
;(factor-tree 60)

;;; 8.71
;          A
;        /   \
;       B     E
;     /  \
;    C    E
;   /
;  D

;;; 8.72
; + The permissions system at the seminary.
; + The org-chart of a company.
; + The AST of a computer program.
; + LISP-structured languages.
