#lang racket

(define (factorielle_20 a)
    (cond [(> a 0) (* a (factorielle_20 (sub1 a)))]
        [else 1]))
(factorielle_20 20)
;;; Exercice 1 ->

;----------------------------- Paerie 3 -----------------------------
(define (devine)
  (define a (random 10))
  (define (dev)
    (define c (read))
    (cond ((= a c) (display "gagné") (newline))
          ((> a c) (display "Trop bas") (newline) (dev))
           ((< a c) (display "Trop haut") (newline) (dev))))
  (dev))
;;; Devine