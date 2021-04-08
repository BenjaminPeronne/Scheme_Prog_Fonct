#lang racket

; fonctions utiles
(define (iota a b)
  (if (> a b) '()
  (cons a (iota (+ a 1) b))))

(define ($append L1 L2)
  (cond ((null? L1) L2)
        (else (cons (car L1) ($append (cdr L1) L2)))))

($append '(do re mi) '(fa sol si la re))


(define ($reverse L)
  (define (aux L LR)
    (cond ((null? L) LR)
          (else (aux (cdr L) (cons (car L) LR)))))
  (aux L '()))

($reverse '(do re (mi fa) sol))


(define ($list-ref L k)
  (cond ((zero? k) (car L))
        (else ($list-ref (cdr L) (sub1 k)))))

($list-ref (iota 5 10) 3)

(define ($k-premier L k)
  (cond ((zero? k) '())
        (else (cons (car L) ($k-premier (cdr L) (sub1 k))))))

($k-premier  '(fa sol si la re) 3)

;tri insertion
(define (tri-ins L)
  (define (insere x L)
    (cond ((null? L) (cons x L))
          ((> x (car L)) (cons (car L) (insere x (cdr L))))
          (else (cons x L))))
    (if (null? L)
        L
        (insere (car L) (tri-ins (cdr L)))))

; test de la fonction
'(tri-ins '(5 3 7 2 1 3 4))
(tri-ins '(5 3 7 2 1 3 4))

; fonction séparant en deux sous liste une liste
(define (scission L)
  (define (aux L L1 L2)
    (if (null? L)
      (cons L1 (cons L2 '()))
      (if (null? (cdr L))
          (aux (cdr L) (cons (car L) L1) L2)
          (aux (cddr L) (cons (car L) L1) (cons (cadr L) L2)))))
  (aux L '() '()))

; test de la fonction
'(scission '(7 3 5 9 2 1 8 0 4))
(scission '(7 3 5 9 2 1 8 0 4))

; fusion de deux sous liste triees
(define (fusion LT1 LT2)
  (cond ((null? LT1) LT2)
        ((null? LT2) LT1)
        ((< (car LT1) (car LT2)) (cons (car LT1) (fusion (cdr LT1) LT2)))
        (else (cons (car LT2) (fusion LT1 (cdr LT2))))))
;test de la fonction
'(fusion '(2 4 5 7 8) '(0 1 3 9))
(fusion '(2 4 5 7 8) '(0 1 3 9))

;tri fusion
 (define (tri-fusion L)
  (if (or (null? L) (null? (cdr L)))
      L
      (let* ((S (scission L)) (L1 (car S)) (L2 (cadr S)))
        (fusion (tri-fusion L1) (tri-fusion L2)))))
;test de la fonction
'(tri-fusion '(7 3 5 9 2 1 8 0 4))
(tri-fusion '(7 3 5 9 2 1 8 0 4))

;; from : http://codeimmersion.i3ci.hampshire.edu/2009/09/20/a-simple-scheme-shuffle/
(define shuffle ; Returns a randomly re-ordered copy of list.
  (lambda (list)
    (if (< (length list) 2) 
        list
        (let ((item (list-ref list (random (length list)))))
          (cons item (shuffle (remove item list)))))))

