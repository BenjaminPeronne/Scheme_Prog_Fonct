#lang racket

(define (moyenne a b)
  (cond ((and (number? a) (number? b)) (/ (+ a b) 2))
        (else (error "not numbers"))))

(moyenne 2 3)
;(moyenne 'a "tt")

(define (moy-inter)
  (moyenne (read) (read)))
;(moy-inter)

(define (dix-fois)
  (display "Programmer, c'est calculer")(newline)
  (display "Programmer, c'est calculer")(newline)
  (display "Programmer, c'est calculer")(newline)
  (display "Programmer, c'est calculer")(newline)
  (display "Programmer, c'est calculer")(newline)
  (display "Programmer, c'est calculer")(newline)
  (display "Programmer, c'est calculer")(newline)
  (display "Programmer, c'est calculer")(newline)
  (display "Programmer, c'est calculer")(newline)
)


(dix-fois)

(define (dix-foisbis)
  (define (n-fois n)
    (display "Programmer, c'est calculer 2")(newline)
    (cond ((= n 1 ) 0 )
          (else (n-fois (- n 1)))))
  (n-fois 10))
(dix-foisbis) 
 
(define (get-number) 
  (let ((a (read)))
    (cond ((number? a) a)
          (else (get-number)))))
;(get-number)

(define (euclide a b)
  (cond ((zero? b) a)
        (else (euclide b (modulo a b)))))

;(euclide 5 15)
;(euclide 17 21)

(define (devine)
  (define (dev a)
    (cond ((= a (read)) (display "gagné") (newline))
          (else (display "essaye encore") (newline) (dev a))))
  (dev (random 10)))

(define (n-premiers-carres n)
  (cond ((zero? n) (display n)(newline))
        (else (display (sqr n)) (newline) 
              (n-premiers-carres (- n 1)))))

(n-premiers-carres 10)
(define (n-premiers-carres2 n)
  (define (aux i)
    (cond ((= i n) (display (sqr i))(newline))
          (else (display (sqr i)) (newline) 
                (aux (+ i 1)))))
  (aux 0))
(n-premiers-carres2 10)

(define (fact n)
  (cond ((zero? n) 1)
        (else (* n (fact (- n 1))))))
(fact 20)

(define ($abs n)
  (cond ((< n 0) (- n))
        (else n)))
($abs 20.1)
($abs -3.2)

(define (rond f1 f2)
  (lambda (x) (f1 (f2 x))))

(define (add2 x)
  (+ 2 x))
(define add4 (rond add2 add2))
(add4 3)

(define (fib n)
  (cond ((zero? n) 1)
        ((= n 1)   1)
        (else (+ (fib (- n 1)) (fib (- n 2))))))
(fib 5)

(define (binom n p)
  (cond ((zero? p) 1)
        ((> p n) 0)
        (else (+ (binom (- n 1) p) (binom (- n 1) (- p 1))))))

 (binom 4 2)

;; bonus zone 
(map (lambda (x) (binom 4 x)) '(0 1 2 3 4))
(apply + (map (lambda (x) (binom 4 x)) '(0 1 2 3 4)))
(map random ((letrec ((f (lambda (n x) (if (zero? n) '() (cons x (f (- n 1) x)))))) f) 10 1))
(map random ((letrec ((f (lambda (n x) (if (zero? n) '() (cons x (f (- n 1) x)))))) f) 10 2))