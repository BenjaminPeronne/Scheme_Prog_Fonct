#lang racket

;; définitions utiles pour afficher-evaluer des expressions 
(define-namespace-anchor anc)
(define ns (namespace-anchor->namespace anc))
(define (print-and-eval E)
  (display E) (display " ~~> ") 
  (eval E ns))

(define (print-and-eval-ln E)
  (display E) (display " ~~> ")(newline)
  (eval E ns))

(define (un-sur-deux L)
  (cond ((null? L) L)
        ((null? (cdr L)) L)
        (else (cons (car L) (un-sur-deux (cddr L))))))

(print-and-eval '(un-sur-deux' (a b c d e)))
(print-and-eval '(un-sur-deux' (a)))

(define (un-sur-n L n)
  (define (iter i L)
    (cond ((null? L) L)
          ((zero? (modulo i n)) (cons (car L) (iter (add1 i) (cdr L))))
          (else (iter (add1 i) (cdr L)))))
  (iter 0 L))

(print-and-eval '(un-sur-n' (a b c d e) 3))
(print-and-eval '(un-sur-n' (a b) 3))

(define (somme L)
  (cond ((null? L) 0)
        (else (+ (car L) (somme (cdr L))))))

(print-and-eval '(somme '(1 2 3)))

(define (somme-ponderee L P)
  (cond ((null? L) 0)
        (else (+ (*(car P) (car L)) (somme-ponderee (cdr L)(cdr P))))))


(print-and-eval '(somme-ponderee '(1 2 3) '(0.25 0.25 0.5)))
(print-and-eval '(somme-ponderee '(1 2 3) '(1/4 1/4 1/2)))

(define (premier? n)
  (define (iter i)
    (cond ((> i (sqrt n)) #t)
          ((zero? (modulo n i)) #f)
          (else (iter (add1 i)))))
  (iter 2))(listeconsecutif? '( 2 3 4)) 

(print-and-eval '(premier? 15))
(print-and-eval '(premier? 157))

(define (facteurs-premiers n)
  (define (iter i)
    (cond ((>= i  n) (list n))
          ((zero? (modulo n i)) (cons i (facteurs-premiers (quotient n i))))
          (else (iter (add1 i)))))
  (iter 2))

(print-and-eval '(facteurs-premiers 15))
(print-and-eval '(facteurs-premiers 157))
(print-and-eval '(facteurs-premiers 256))

(define (prefixe? L P)
  (cond ((null? P) #t)
        ((null? L) #f)
        ((equal? (car L) (car P)) (prefixe? (cdr L) (cdr P)))
        (else #f)))

(print-and-eval '(prefixe? '(a b c d) '(a b)))
(print-and-eval '(prefixe? '(a b c d) '(a d)))
(print-and-eval '(prefixe? '(a b) '(a b c d)))

(define (suffixe? L P)
  (prefixe? (reverse L) (reverse P)))

(print-and-eval '(suffixe? '(a b c d) '(c d)))
(print-and-eval '(suffixe? '(a b c d) '(a c d)))

(define (palindrome? L)
  (equal? L (reverse L)))

(print-and-eval '(palindrome? '(n o n)))
(print-and-eval '(palindrome? '(r a d a r)))
(print-and-eval '(palindrome? '(o u i)))

(define (genere-liste e n)
  (cond ((zero? n) '())
        (else (cons e (genere-liste e (sub1 n))))))

(print-and-eval '(genere-liste 'a 4))
(print-and-eval '(genere-liste 'a 0))
(print-and-eval '(genere-liste 1 6))