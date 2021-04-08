#lang racket

(define (listeconsecutif? L) 
    (cond ((null? L) #t)
          ((null? (cdr L)) #t)
          ((= (add1 (car L)) (cadr L)) (listeconsecutif? (cdr L)))
          (else #f))) 

(define (triee? L)
  (cond ((null? L) #t)
        ((null? (cdr L)) #t)
        ((< (car L) (cadr L)) (triee? (cdr L)))
        (else #f)))


(define (somme L)
    (cond ((null? L) 0)
          ((apply * L))))

(define (produit L)
    (cond ((null? L) 0)
          ((apply * L))))


(define (listenombre? L)
    (cond ((null? L) #t)
          ((number? (car L)) (listenombre? (cdr L)))
          (else #f)))

(define (puissance2? a)
    (cond ((zero? a) #f)
          ((even? a) (puissance2? (/ a 2)))
          ((= 1 a) #t)
          (else #f)
    ))

(define (divise? a b)
    (cond ((or (zero? a) (zero? b)) #f)
          ((= a b) #t)
          ((and (even? a) (even? b)) #t)
          ((and (even? a) (odd? b)) #f)
          ((and (odd? a) (even? b)) #f)
          ((and (odd? a) (odd? b)) #f)))



(define (listepairs L)
    (cond ((null? L) L)
          ((even? (car L)) (cons (car L) (listepairs (cdr L))))
          (else (listepairs (cdr L)))))


(define (sous-liste L i k)
    (define (aux L c)    
        (cond ((null? L) L)
              ((and (>= c i) (<= c k)) (cons (car L) (aux (cdr L) (add1 c))))
              (else (aux (cdr L) (add1 c)))))
    (aux L 0))


(define (enleve-sous-liste L i k)
    (define (aux L c)    
        (cond ((null? L) L)
              ((not (and (>= c i) (<= c k))) (cons (car L) (aux (cdr L) (add1 c))))
              (else (aux (cdr L) (add1 c)))
        )
    )
    (aux L 0))

(define (f-mul a)
    (define (mult n)
        (* a n)
    )
    mult)

(define (f-div a)
    (define (div n)
        (/ a n)
    )
    div)