s#lang sicp
(#%require sicp-pict)
;(require (planet dyoo/simply-scheme:2:2))
(define (identity x) x)

(define (inc x) (+ x 1))

(define (dec x) (- x 1))


;; 1.1
; 10
; 12
; 3
; 6
; u a is 3
; u b is 4
; 19
; #f
; 3
; 16
; 6
; 16

;; 1.2

; (/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))

;; 1.3
;

(define p
  (lambda (a b c)
    (define (square x) (* x x))
    (define (sum-square x y) (+ (square x) (square y)))
    (cond ((and (> a c) (> b c)) (sum-square a b))
          ((and (> a b) (> c b)) (sum-square a c))
          (else (sum-square b c)))))

; (check-expect (p 5 2 1) 29)
; (check-expect (p 0 0 0) 0)
; (check-expect (p 0 0 4) 16)
;; 1.4
; when the compound proc evaluated we get an operator depending on sign of b, which
; plays the role of abs

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

;; with aplicative order, we get stuck in a loop, since we apply the operands after evaluating, and (p) evaluates to itself, while in normal order when reducing after contracting the body of test we only apply
;; 0 inside the if predicate

;; 1.6

;; we get stuck in loop because of the applicative order, when evaluating the call to sqr-iter

;; 1.7

;; xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

;; 1.8
(define (square x) (* x x))



(define (average x y) (/ (+ x y) 2))

(define (cube-r x)
  (define (improve g)
    (/ (+ (/ x (* g g)) (* 2 g)) 3))

  (define (cube-r-iter g)
    (if (good-enough? g)
        g
        (cube-r-iter (improve g))))

  (define (good-enough? g)
    (< (abs (- (cube g) x)) 0.001))

  (cube-r-iter 1.0))

;(define (cube x) (* x x x))

;; 1.9
;; ----------------------------
; recursive process
;(+ 4 5)
;(inc (+ 3 5))
;(inc (inc (+ 2 5)))
;(inc (inc (inc (+ 1 5))))
;(inc (inc (inc (inc (+ 0 5)))))
;(inc (inc (inc (inc 5))))
;(inc (inc (inc 6)))
;(inc (inc 7))
;(inc 8)
;9

;; ----------------------------------
;; iterative process
;(+ 4 5)
;(+ 3 6)
;(+ 2 7)
;(+ 1 8)
;(+ 0 9)
;9

;; Ex 1.10

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

; (A 1 10) -> 2 ^ 16

;(A 2 4)
;(A 1 (A 2 3))
;(A 1 (A 1 (A 2 2)))
;(A 1 (A 1 (A 1 (A 1 1))))
;(A 1 (A 1 (A 1 2)))
;(A 1 (A 1 (A 0 (A 1 1))))
;(A 1 (A 1 (A 0 2)))
;(A 1 (A 1 4))
;(A 1 16) -> 2 ^ 16

;(A 3 3) -> (A 2 4) -> (A 1 16)


;(A 2 3) -> (A 1 4)


;; f -> 2n
(define (f n) (A 0 n))

;; g -> 2 ^ n
(define (g n) (A 1 n))

;; h -> 2 ^ n ^2
(define (h n) (A 2 n))


;; Ex 1.11
(define (fn n)
  (cond ((< n 3) n)
        (else (+ (fn (- n 1))
                 (* 2 (fn (- n 2)))
                 (* 3 (fn (- n 3)))))))

(define (fn-i n)
  (define (helper c three two one)
    (if (= c 0)
        one
        (helper (- c 1) two one (+ one (* 2 two) (* 3 three)))))
  (if (< n 3)
      n
      (helper (- n 2)  0  1  2)))


;; Ex 1.15

(define (cube x) (* x x x))

(define (p x) (- (* 3 x) (* 4 (cube x))))

(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

; p is applied 5 times
; space and time O(log n)

;; Ex 1.16

(define (fast-exp b n)
  (define (helper b n a)
    (cond ((= n 0) a)
          ((even? n) (helper (square b) (/ n 2) a))
          (else (helper b (- n 1) (* a b)))))
  (helper b n 1))

;; Ex 1.17

(define (double a) (* a 2))
(define (halve a) (/ a 2))

(define (fast-mul a b)
  (cond
    ((= b 0) 0)
    ((even? b) (double (fast-mul a (halve b))))
    (else (+ a (fast-mul a (- b 1))))))

;; Ex 1.18

(define (mul-iter a b)
  (define (helper a b r)
    (cond ((= b 1) r)
          ((even? b) (helper a (halve b) (double r)))
          (else (helper a (- b 1) (+ a r)))))
  (helper a b a))

;; Ex 1.19

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (* q q) (* p p))
                   (+ (* q q) (* p q 2))
                   (/ count 2)))
        (t (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

;; Ex 1.20

;(gcd 206 40)
;(gcd 40 (remainder 206 40))
;(gcd (remainder 206 40) (remainder 40 (remainder 206 40) ))
;(gcd (remainder 40 (remainder 206 40) ) (remainder (remainder 206 40) (remainder 40 ;(remainder 206 40) ) ))


;; in applicative order 4, in normal order 24 times

;; 0 1
;; 1 2
;; 2 4
;; 4 7  total-left = 14

;-> result (since only the total of right side get applied + plus the last one in left side) = 14 + 4 = 18 times


;; Ex 1.21

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))

; (smallest-divisor 199) -> 199
; (smallest-divisor 1999) -> 1999
; (smallest-divisor 19999) -> 7

;; Ex 2.1

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g)))
  [if (< d 0)
      (cons (/ (- n) g) (/ (- d) g))
      (cons (/ n g) (/ d g))])


; Ex 2.4

(define (cons2 x y)
  (lambda (m) (m x y)))

(define (car2 z)
  (z (lambda (p q) p)))

(define (cdr2 z) (z (lambda (p q) q)))

; Ex 2.5

(define ddd 3)


(define (cons3 x y)
  (* (expt 2 x) (expt 3 y)))


(define (extract-expt n d)
  (define (iter try-expt)
    (if (zero? (remainder n (expt d try-expt)))
        (iter (inc try-expt))
        ( try-expt)))
  (iter 1))

(define (car3 x) (extract-expt x 2))
(define (cdr3 x) (extract-expt x 3))

; Ex 2.6


(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one (lambda (f) (lambda (x) (f x))))

(define two (lambda (f) (lambda (x) (f (f x)))))

(define three (lambda (f) (lambda (x) (f (f (f x))))))

(define (add-f a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))

; Ex 2.7

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(define (make-interval a b) (cons a b))

(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

(define (sub-interval x y)
  (make-interval (-  (lower-bound y) (lower-bound x))
                 (- (upper-bound x) (upper-bound y))))



;; Ex 2.9


; we have width of i3 is 1/2 (up3 + lo3) where i3 is i1 + i2
; width of (i1 + i2) is also width of i1 + width of i2
; 1/2 (up1 - low1) + 1/2 (up2 - low2)  = 1/2 ((up1 + up2 = up3) - (low1 + low2 = low3)))

; for multiplication it doesn't work because we don't preserve the precedence of upper and
; lower limits of an interval. example [2 3] is 1/2 and [1 3] is 1 if we multiply we get
; [2 9] with width 3.5 and not 1/2

;; Ex 2.10


(define (div-interval2 x y)
  (if (>= (* (/ 1.0 (upper-bound y))
             (/ 1.0 (lower-bound y))) 0)
      (display "ERROR")
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))



;; Ex 2.11
#;
(define (mult-interval3 x y)
  (define (positive? x)
    (and (>= (lower-bound x) 0) (>= (upper-bound x) 0)))
  (define (negative? x)
    (and (< (lower-bound x) 0)
         (< (lower-bound x) 0)))
  (define (span? x)
    (and (< (lower-bound x) 0) (> (upper-bound x) 0)))

  (define up-x (upper-bound x))
  (define up-y (upper-bound y))
  (define low-x (lower-bound x))
  (define low-y (lower-bound y)))

;; Ex 1.29

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (simpson  f a b n)
  (define h (/ (- b a) n))
  (define (iter k)
    (cond ((or (= k 0) (= k n))
           (f (+ a (* k h))))
          ((even? k) (* 2 (f (+ a (* k h)))))
          (else (* 4 (f (+ a (* k h)))))))
  (* (/ h 3.0) (sum2 iter 0.0 inc n)))

;; Ex 1.30

(define (sum2 term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

;; X  Ex 1.31

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (product2 term a next b)
  (define (iter a r)
    (if (> a b)
        r
        (iter (next a) (* (term a) r))))
  (iter a 1))

(define (factorial2 n)
  (product (lambda (x) x) 1 inc n))

(define pi-computed (* 4.0 (product (lambda (x) (if (even? x)
                                               (/ (+ x 2) (+ x 1))
                                               (/ (+ x 1) (+ x 2))))
                                    1 inc 100)))



;; Ex 1.32



;; (define (accumulate combiner null-value term a next b)
;;   (if (> a b)
;;       null-value
;;       (combiner (term a)
;;                 (accumulate combiner null-value term (next a) next b))))

(define (sum3 term a next b)
  (accumulate2 + 0 term a next b))



(define (accumulate2 combiner null-value term a next b)
  (define (iter a r)
    (if (> a b)
        r
        (iter (next a) (combiner r a))))
  (iter a null-value))

;; Ex 1.33

(define (filter-accumulate combiner null-value term a next b predicate)
  (define (iter a r)
    (if (> a b)
        r
        (iter (next a) (if (predicate a)
                           (combiner r a)
                           r))))
  (iter a null-value))

(define (filtered-accumulate3 combiner null-value term a next b filter)
  (define (iter a result)
    (cond ((> a b) result)
          ((filter a) (iter (next a) (combiner result (term a))))
          (else (iter (next a) result))))
  (iter a null-value))

(define (filter-accumulate2 combiner null-value term a next b predicate)
  (cond ((> a b)
         null-value)
        ((predicate a)
         (combiner (term a)
                   (filter-accumulate combiner null-value term (next a) next b predicate)))
        (else (filter-accumulate combiner null-value term (next a) next b predicate))))

(define (smallest-div n)
  (define (divides? a b)
    (= 0 (remainder b a)))
  (define (find-div n test)
    (cond ((> (square test) n) n)
          ((divides? test n) test)
          (else (find-div n (+ test 1)))))
  (find-div n 2))

(define (prime? n)
  (if (= n 1) false (= n (smallest-div n))))


(define (sum-squares-primes a b)
  (filter-accumulate2 + 0 square a inc b prime?))

(define (product-<-n-relative n)
  (filter-accumulate2 * 1 (lambda (x) x) 1 inc (dec n) (lambda (x) (= (gcd x n) 1))))

;; Ex 1.34

(define (f2 g) (g 2))


;; Ex 1.35
;; Ex 1.36  both solved but Emacs decided to close and no backup, so it's gone :(

; (f2 f2)
; (f2 2)
; (2 2) results in applying 2 to 2


;; Ex 1.37

(define (count-frac n d k)
  (if (= k 1)
      (/ (n k) (d k))
      (* (if (= (dec k) 1)
             (d (dec k))
             1)
         (/ (count-frac n d (dec k))
            (+ (if (= k 1)
                   0
                   (d (dec k))) (/ (n k) (d k)))))))

(define (cont-frac2 n d k)
  (define (frac-rec i)
    (/ (n i)
       (+ (d i)
          (if (= i k)
              0
              (frac-rec (+ i 1))))))
  (frac-rec 1))

(define (cont-frac3 n d k)
  (define (iter i r)
    (if (> i k)
        r
        (iter (inc i)
              (/ (n i) (+ r (d i))))))
  (iter 1 0))

(define (cont-fact4 n d k)
  (define (recur i)
    (if (> i k)
        0
        (/ (n i) (+ (d i) (recur (inc i))))))
  (recur 1))

;; Ex 1.38

(define (approx-e k)
  (define (d i)
    (cond ((= i 2) i)
          ((= (modulo i 3)  2) (* 2 (ceiling (/ i 3.0))))
          (else 1)))
  (+ 2 (cont-fact4 (lambda (x) 1.0)
                   d k)))

;; Ex 1.39

(define (cont-fact-op  n d k op)
  (define (recur i)
    (if (> i k)
        0
        (/ (n i) (op (d i) (recur (inc i))))))
  (recur 1))

(define (tan-cf x k)
  (define (d i)
    (dec (* 2 i)))
  (define (n i) (if (= i 1) x (square x)))
  (cont-fact-op  n d k -))

(define (tan-cf2 x k)
  (define (d i)
    (dec (* 2 i)))
  (define (n i) (if (= i 1) x (- (square x))))
  (cont-fact4  n d k))


(define newton-transfom
  (lambda (g)
    (lambda (x) (- x (/ (g x)
                   ((deriv g) x))))))
(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x))
            dx)))

(define dx 0.00001)

(define (newton-method g guess)
  (fixed-point (newton-transfom g)
               guess))

(define (fixed-point f guess)
  (define (try g)
    (if (close? (f g) g)
        (f g)
        (try (f g))))
  (try guess))

(define (close? u v) (< (abs (- u v)) 0.0001))

(define (cubic a b c)
  (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))

; Ex 1.41

(define (double2 p)
  (lambda (x)
    (p (p x))))
; inc applied to double -> +2 then to double +4 -> double +8 -> double +16

;(((double2 (double2 double2)) inc) 5)
;(((double2 (Î» (x) (double2 (double2 x)))) inc) 5)

;((Î» (x)
;   (double2 (double2 (double2 (double2 x)))) inc) 5)
; (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc 5))))))))))

; Ex 1.42

(define (compose f g)
  (lambda (x) (f (g x))))

;; Ex 1.43

(define (repeated2 f n)
  (if (= n 1)
      (lambda (x) (f x))
      (lambda (x) ((repeated2 f (dec n)) (f x)))))

(define (repeated f n)
  (if (= n 1)
      f
      (compose (repeated f (dec n)) f)))

(define (repeated-iter f n)
  (define (iter n r)
    (if (= n 1)
        r
        (iter (dec n) (compose (repeated f r)))))
  (iter n f))


(define (repeated-rec f n)
  (if (= n 0)
      (lambda (x) x)
      (compose f (repeated-rec f (- n 1)))))

;; Ex 1.44

(define (smooth f)
  (lambda (x) (/
          (+ (f (- x dx)) (f x) (f (+ x dx)))
          3)))



(define (n-fold-somooth f n)
  ((repeated smooth n)) f)

; Ex 1.45

(define (average-damp f)
  (lambda (x) (/ (+ (f x) x) 2)))



(define (nth-root x n)
  (fixed-point
   ((repeated
     average-damp
     (floor (/ n 2.0)))
    (lambda (y) (/ x (expt (dec n) y))))
   1.0))

(define (log2 x) (/ (log x) (log 2)))

(define (nth-root2 n x)
  (fixed-point ((repeated average-damp (floor (log2 n)))
                (lambda (y) (/ x (expt y (- n 1)))))))

;; Ex 1.46
(define (itetrative-improv good-enough? improv)
  (lambda (guess)
    (define (iter g)
      (if (good-enough? g)
          g
          (iter (improv g))))
    (iter guess)))



(define (sqrt_ x)
  ((itetrative-improv
    (lambda (g)  (< (abs (- (square g) x)) 0.001))
    (lambda (y) (/ (+ (/ x y) y) 2))) 1.0))



(define (last-pair non-nil)
  (if (null? (cdr non-nil))
      non-nil
      (last-pair (cdr non-nil))))


; Examples
(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

;; Ex 2.18

(define (reverse l)
  (if (null? l)
      nil
      (append (reverse (cdr l)) (list (car l)))))

;; Ex 2.19

;; Example

(define (count-change amount)
  (cc amount 5))


(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

(define (cc2 amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc2 amount
                 (except-first-denomination coin-values))
            (cc2 (- amount
                    (first-denomination2 coin-values))
                 coin-values)))))

(define (first-denomination2 coin-values)
  (car coin-values))

(define us-coins (list 50 25 10 5 1))

(define (except-first-denomination l) (cdr l))

(define (no-more? l) (null? l))

;; the order doesn't count because we are adding the possible combinations

;; Ex 2.20

(define (same-parity . w)
  (define (recur l check)
    (cond ((null? l) nil)
          ((check (car l))
           (cons (car l) (recur (cdr l) check)))
          (else (recur (cdr l) check))))

  (cons (car w) (recur (cdr w) (if (even? (car w)) even? odd?))))

;; Ex 2.21

(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list (cdr items)))))

(define (square-list2 items)
  (map square items))



;; Ex 2.22

(define (square-list3 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items nil))
;; cause we are consing in front as we go throw the list

(define (square-list4 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items nil))

;; we will be consing the entire list with a pair

;; Ex 2.23

(define (for-each f l)
  (cond ((not (null? l))
         (f (car l)) (for-each f (cdr )))))

;;  Example

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

;; Ex 2.24

;(list 1 (list 2 (list 3 4)))
; is a tree with 2 branches the right one is 1 and left one is
;; a subtree with 2 branches, where the left is 3 and right 4

;; (1 (2 (3 4)))
; 1           (2 (3 4))
;;         2         (3 4)
;;                  3       4

;; Ex 2.25

(define l1 (list 1 3 (list 5 7) 9))
(car (cdr (car (cdr (cdr l1)))))
;(define l2 ((7)))
;(car (car l2))

;; Ex 2.26

;; (append x y) -> (list 1 2 3 4 5 6)
;; (cons x y)   -> (list (list 1 2 3) 4 5 6)
;; (list x y)   -> (list (list 1 2 3) (list 3 5 6))

;; Ex 2.27

(define (deep-reverse l)
  (cond ((null? l) nil)
        ((not (pair? l)) l)
        (else (append (deep-reverse (cdr l))
                      (list (deep-reverse (car l)))))))

(define x (list (list 1 2) (list 3 4)))

;; Ex 2.28

(define (fringe l)
  (cond ((null? l) nil)
        ((not (pair? l)) (list l))
        (else (append (fringe (car l))
                      (fringe (cdr l))))))

;; Ex 2.29

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch bm) (car bm))
(define (right-branch bm) (car (cdr bm)))

(define (branch-length b) (car b))
(define (branch-structure b) (car (cdr b)))

(define (total-weight m)
  (+ (branch-weight (left-branch m))
     (branch-weight (right-branch m))))

(define (branch-weight b)
  (let ((s (branch-structure b)))
    (if (pair? s) (total-weight s) s)))

(define level-1-mobile (make-mobile (make-branch 2 1)
                                    (make-branch 1 2)))
(define level-2-mobile (make-mobile (make-branch 3 level-1-mobile)
                                    (make-branch 9 1)))
(define level-3-mobile (make-mobile (make-branch 4 level-2-mobile)
                                    (make-branch 8 2)))

(define (balanced? bm)
  (and (= (branch-torque (left-branch bm))
          (branch-weight (right-branch bm)))
       (balanced? (left-branch bm))
       (balanced? (right-branch bm))))

(define (branch-torque b)
  (* (branch-length b) (branch-weight b)))


;; we need  to change the accessors  with this representation

;; (define (make-mobile left rppight)
;;   (cons left right))
;; (define (make-branch length structure)
;;   (cons length structure))

;; (define (left-branch bm) car)
;; (define (right-branch bm) cdr)

;; (define (branch-length b) car)
;; (define (branch-structure b) cdr)

;; Ex 2.30

(define (scale-tree tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree sub-tree factor)
             (* sub-tree factor)))
       tree))
(define (scale-tree2 tree factor)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree2 (car tree) factor)
                    (scale-tree2 (cdr tree) factor)))))
;; Ex 2.30

(define (square-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))
(define (square-tree2 tree)
  (map (lambda (subtree)
         (if (pair? subtree)
             (square-tree2 subtree)
             (square subtree)))
       tree))

(define (tree-map op tree)
  (map (lambda (subtree)
         (if (pair? subtree)
             (tree-map op subtree)
             (op subtree)))))

;; Ex 2.32

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (c) (cons (car s) c)) rest)))))

;; it works because the subsets of a set is a combination of it's elements
;; so the first element added to the set of the subsests of the rest of our set

;; Ex 2.33


;; Examples
(define (accumulate-s op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate-s op initial (cdr sequence)))))



(define (filter-s predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter-s predicate (cdr sequence))))
        (else (filter-s predicate (cdr sequence)))))

(define (map-s p sequence)
  (accumulate-s (lambda (x y) (cons (p x) y)) nil sequence))

(define (append-s seq1 seq2)
  (accumulate-s cons  seq2 seq1))

(define (length-s sequence)
  (accumulate-s (lambda (x y) (inc y)) 0 sequence))

;; Ex 2.34

(define (horner-eval x coefficient-sequence)
  (accumulate-s (lambda (this-coeff higher-terms)
                  (+ this-coeff (* higher-terms x)))
                0
                coefficient-sequence))

;; Ex 2.35

(define (count-leaves-s t)
  (accumulate-s + 0 (map (lambda (x) (if (pair? x)
                                    (length-s x)
                                    1)) t)))

;; Ex 2.36

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate-s op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

;; Ex 2.37

;; Example

(define (dot-product v w)
  (accumulate-s + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product v x)) m))

(define (transpose mat)
  (accumulate-n cons nil mat)) ;?

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector cols x)) m)))

;; Ex 2.38

(define fold-right accumulate-s)

(define fold-left
  (lambda (op init seq)
    (define (iter r rest)
      (if (null? rest)
          r
          (iter (op r (car rest))
                (cdr rest))))
    (iter init seq)))

;; (fold-right / 1 (list 1 2 3)); => 2/3
;; (fold-left / 1 (list 1 2 3)); => 1/6
;; (fold-right list nil (list 1 2 3));  => ;(1 (2 (3 '())))
;; (fold-left list nil (list 1 2 3)) ;  => ;; (((nil 1) 2) 3)

; (op a b) is the same as (op b a) needed as property so fold-righ and
; fold-left give the same result

;; Ex 2.39

(define (reverse-with-fold-right sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))

(define (reverse-with-fold-left sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))


;; Example

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

;; Ex 2.40

;; Example
(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))


(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter-s prime-sum?
                 (unique-pairs n))))

(define (flatmap proc seq)
  (accumulate-s append nil (map proc seq)))


(li)


(define (permutations s)
  (if (null? s)                    ; empty set?
      (list nil)                   ; sequence containing empty set
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))


(define (remove item sequence)
  (filter-s (lambda (x) (not (= x item)))
            sequence))

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j)
                    (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

;; Ex 2.41

(define (unique-triples n)
  (flatmap (lambda (i)
             (flatmap (lambda (j)
                        (map (lambda (k) (list i j k))
                             (enumerate-interval 1 (- j 1))))
                      (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))


;; Ex 2.42 TODO

;; (define (queens board-size)
;;   (define (queen-cols k)
;;     (if (= k 0)
;;         (list empty-board)
;;         (filter-s
;;          (lambda (positions) (safe? k positions))
;;          (flatmap
;;           (lambda (rest-of-queens)
;;             (map (lambda (new-row)
;;                    (adjoin-positon  new-row k rest-of-queens))
;;                  (enumerate-interval 1 board-size)))
;;           (queen-cols (- k 1))))))
;;   (queen-cols board-size))


;; representation for sets of board positions ;

;; (list (list (list i j) <= a board position.... ) <= a set of bard
;; position ) <= sets of bard positons

(define empty-board nil) ; <= a nil positon

;; (define (adjoin-positon new-row k rest-of-queens)
;;   (cons (make-pos row col) res-of-queens ))

(define (make-pos row col) cons)        ;


;; (define (safe? k positons)
;;   (accumulate-s  (lambda (cur res)
;;                    (if (position= curr k-pos)
;;                        res
;;                        (and (safe-from? k-pos cur))))  #t))

(define (map-iter p l)
  (define (iter nl r)
    (if (null? nl)
        r
        (iter (cdr l)
              (append r (list (p (car l)))))))
  (iter l nil))


(define (print-list-structure x)
  (define (print-contents x)
    (print-list-structure (car x))
    (cond ((null? (cdr x)) nil); end of list
          ((not (pair? (cdr x)))       ; not a list
           (display " . ")
           (print-list-structure (cdr x)))
          (else
           (display " ")
           (print-contents (cdr x)))))

  (cond ((null? x) (display "()"))
        ((not (pair? x)) (display x))
        (else (display "(")
              (print-contents x)
              (display ")"))))


(print-list-structure '(1 2 3))



;; lib 	liberal-define-context? 	link 	link-exists?
;; list 	list* 	list*of 	list->bytes
;; list->mutable-set 	list->mutable-seteq 	list->mutable-seteqv 	list->set
;; list->seteq 	list->seteqv 	list->string 	list->vector
;; list->weak-set 	list->weak-seteq 	list->weak-seteqv 	list-contract?
;; list-prefix? 	list-ref 	list-set 	list-tail
;; list-update 	list/c 	list? 	listen-port-number?
;; listof
