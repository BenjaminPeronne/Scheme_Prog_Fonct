#lang racket

(define (moyenne a b)
  (cond ((and (number? a) (number? b)) (/ (+ a b) 2))
        (else (error "not numbers"))))

;;; (moyenne 2 3)
;(moyenne 'a "tt")
