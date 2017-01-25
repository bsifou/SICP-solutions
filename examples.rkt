#lang sicp

(define (average x y) (/ (+ x y) 2))
(define (square x) (* x x))

(define quadratic-root
  (lambda (a b c)
    (define s (lambda (si) (/ (si (- b) (sqrt (- (* b b) (* 4 a c)))) (* 2 a))))
    (define (max a b) (if (> a b) a b))
    (max (s +)
         (s -))))

(define (try x1 x2)
  (+ x1 2)
  (+ x2 2))

(define max (lambda ( a b) (if (> a b) a b)))

(define max-sum (lambda (a b) (max (+ a b) (- a b))))

(define fizz (lambda (a b) (* (/ a b) (/ b a))))

(define fuzz (lambda (a b) (if (= b 0) a (/ 1 b))))

(define (f n)
  (cond ((< n 3) n)
        (else (+ (f (- n 1))
                 (* 2 (f (- n 2)))
                 (* 3 (f (- n 3)))))))


(define test (lambda (a b)
               ((if (> b 0) + -) a b)))

(define slow-mul (lambda (a b)
                   (if (or (= a 0)  (= b 0))
                       0
                       (+ b (slow-mul (- a 1) b)))))

(define plus (lambda (a b)
               (if (= a 0)
                   b
                   (if (= b 0)
                       a
                       (+ 1 (plus a (dec b)))))))


(define (recur-pow a b)
  (cond ((= b 0) 1)
        (else (* a (recur-pow a(- b 1))))))

(define (iter-power a b)
  (define (iter n r)
    (if (= n 0) r
        (iter (- n 1) (* r a))))
  (iter b 1))



(define simple-log (lambda (n)
                     (cond ((= n 1) 0)
                           (else (+ 1 (simple-log (/ n 2)))))))

(define (odd? n)
  (define (odd?-iter r)
    (cond ((= r 1) #t)
          ((= r 0) #f)
          (else (odd?-iter (- r 2)))))
  (odd?-iter n))


(define slow-mul-recurse
  (lambda (a b)
    (if (= b 0)
        0
        (+ a (slow-mul-recurse a (- b 1))))))


(define slow-mul-iter
  (lambda (a b)
    (define (helper r n)
      (if (= n 0)
          r
          (helper (+ a r) (-  n 1))))
    (helper 0 b)))


(define fast-mul
  (lambda (a b)
    (cond ((= a 0) 0)
          ((= a 1) b)
          ((even? a) (double (fast-mul (halve a) b)))
          (else (+ b (fast-mul (- a 1) b))))))

(define (double x) (* x 2))
(define (halve x) (/ x 2))

(define quick-sum (lambda (n)
                    (/ (* n(+ n 1)) 2)))


(define fast-expi
  (lambda (a b)
    (define (helper a b r)
      (cond ((= b 0)  r)
            ((even? b) (helper (* a a) (/ b 2) r))
            (else (helper a (- b 1) (* a r)))))
    (helper a b 1)))


;(really-big-hairy-slow-procedure  (if (= a 0) 3 4))

(define (boolean-odd? x)
  (define (helper c)
    (or (= c 1)
        (and (> c 1)
             (boolean-odd? (- c 2)))))
  (helper x))

(define sum-by-halves
  (lambda (a b)
    (if (= a b)
        a
        (+ (sum-by-halves a (floor (/ (+ a b) 2)))
           (sum-by-halves (+ (floor (/ (+ a b) 2)) 1) b)))))


(define mystery
  (lambda (a b)
    (mystery-meat 0 a b)))

(define mystery-meat
  (lambda (c a b)
    (if (> a b)
	c
	(mystery-meat (+ c a) (+ a 1) b))))

(define clarity
  (lambda (a b)
    (if (> a b)
        0
        (+ a (clarity (+ a 1) b)))))

(define (fib n)
  (define (fib-iter p c r)
    (if (= c 0)
        r
        (fib-iter (- p 1) (- c 1) (+ r c))))
  (fib-iter (- n 1) n 0))


(define (fib-recur n)
  (if (< n 2)
      n
      (+ (fib-recur (- n 1))
         (fib-recur (- n 2)))))

(define (list-ref l n)
  (if (zero?  n)
      (car l)
      (list-ref (cdr l) (- n 1))))



(define (append2 l1 l2)
  (if (null? l1)
      l2
      (cons (car l1) (append (cdr l1) l2))))


(define (filter pred lst)
  (cond ((null? lst) '())
        ((pred (car lst))
         (cons (car lst)
               (filter pred (cdr lst))))
        (else (filter pred (cdr lst)))))


(define (generate-interval a b)
  (if (> a b)
      '()
      (cons a (generate-interval (+ 1 a) b))))

(define (fold-right op start lst)
  (if (null? lst)
      start
      (op (car lst)
          (fold-right op start (cdr lst)))))

(define (map f lst)
  (if (null? lst)
      '()
      (cons (f (car lst))
            (map f (cdr lst)))))


(define (sum f a eps n)
  (fold-right +
              0
              (map (lambda (x) (f (+ a (* x eps))))
                   (generate-interval 0 n))))

(define (length los)
  (map string-length los))

(define string-em-up
  (lambda (lst)
    (fold-right + 0 (map string-length (filter string? lst)))))

(define make-multiplier
  (lambda (x) (lambda (y) (* y x))))

(define (close? u v) (< (abs (- u v)) 0.0001))

(define (fixed-point f guess)
  (define (try g)
    (if (close? (f g) g)
        (f g)
        (try (f g))))
  (try guess))

(define (sqrt x)
  (fixed-point
   (lambda (y) (average x (/ x y)))
   1.0))

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((testpoint (f midpoint)))
          (cond  ((positive? testpoint) (search f neg-point midpoint))
                 ((negative? testpoint) (search f midpoint pos-point))
                 (else testpoint))))))

(define (close-enough? x y) (< (abs (- x y)) 0.001))

;; (define (half-interval-method f a b)
;;   (let ((f-a (f a))
;;         (f-b (f b)))
;;     (cond ((and (positive? f-a) (negative? f-b))
;;            (search f b a))
;;           ((and (negative? f-a) (positive? f-b))
;;            (search f b a))
;;           (else (error "values are not of opposite signs...")))))


(define tolerance 0.00001)

(define (fixed-point2 f i-guess)
  (define (close-enough? v1 v2) (< (abs (- v1 v2)) tolerance))

  (define (try guess)

    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try i-guess))



;; Ex 1.35

; we have x2 = x + 1 => x = sqrt x + 1
; we have sqr x = fixed point of y -> x / y hence when we substitute in 1 we find x is fixed point of y -> y + 1 / y

(define phi (fixed-point2 (lambda (x) (+ 1 (/ 1 x))) 1.0))


;; Ex 1.36

(define (fixed-point-prints f i-guess)
  (define (close-enough? v1 v2) (< (abs (- v1 v2)) tolerance))

  (define (try guess)
    (let ((next (f guess)))
      (newline)
      (display next)
      (if (close-enough? guess next)
          next
          (try next))))
  (try i-guess))


(define (average-damp f x) (lambda (x) (/ (+ (f x) x) 2)))

(define (sqrt-fp x) (fixed-point (lambda (y) (average y (/ x y))) 1.0))
(define (sqrt-fp2 x) (fixed-point-prints
                      (average-damp (lambda (y) (/ x y)) x) 1.0))

(define (sol-x-to-x x)
  (fixed-point-prints
   (lambda (y) (/ (log x) (log y)))
   1.0))


(define (sol-x-to-x2 x)
  (fixed-point-prints (average-damp
                       (lambda (y) (/ (log x) (log y))) x)
                      1.0))

(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x))
            dx)))
(define dx 0.00001)


(define newton-transfom
  (lambda (g)
    (lambda (x) (- x (/ (g x)
                   ((deriv g) x))))))


(define (newton g guess)
  (fixed-point (newton-transfom g)
               guess))

(define (sqrt2 x)
  (newton (lambda (y) (- (square y) x)) 1.0))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt5 x)
  (fixed-point-of-transform (lambda (y) (/ x y))
                            average-damp
                            1.0))

(define (sqrt6 x)
  (fixed-point-of-transform (lambda (y) (- (square y) x))
                            newton-transfom
                            1.0))

(define (length1 l)
  (define (iter c l)
    (if (null? l) c (iter (inc c) (cdr l))))
  (iter 0 l))







