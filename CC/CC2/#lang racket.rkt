#lang racket


(define (longueur L)
    (define (aux n l)
        (cond
            ((null? l) n)
            (else (aux (add1 n) (cdr l)))
        )
    )
    (aux 0 L)
)
;;; (longueur '(a b c d)) 


(define (somme L)
    (cond
        ((null? L) 0)
        ((null? (cdr L)) (car L))
        (else (+ (car L) (somme (cdr L))))
    )
)
;;; (somme '(1 2 3 4))


(define (listenombre? L) 
    (cond
        ((null? L) 0)
        ((null? (cdr L)) (car L))
        ((number? L) #t)        
        ((else (number?) #f) (car L) (listenombre? (cdr L)))                
    )
)
;;; (listenombre? '(1 2 3 4)) 
