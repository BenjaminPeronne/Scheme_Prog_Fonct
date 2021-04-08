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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1) fonction doublonne
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (doublonne L)
  (cond ((null? L) L)
        (else (cons (car L) (cons (car L) (doublonne (cdr L)))))))

(display "------------------------------")(newline)
(display "tests fonction doublonne")(newline)
(display "------------------------------")(newline)
(print-and-eval '(doublonne '(a b c)))
(print-and-eval '(doublonne '(a a b c)))
(print-and-eval '(doublonne '(a b a)))
(display "------------------------------")(newline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2) fonction doublonne-exacte
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (doublonne-exacte L)
  (cond ((null? L) L)
        ((null? (cdr L)) (cons (car L)L))
        ((equal? (car L)(cadr L)) (doublonne-exacte (cdr L)))
        (else (cons (car L) (cons (car L) (doublonne-exacte (cdr L)))))))

;; une autre solution : utiliser la fonction dedoublonne_1 du TD précédent
;; moins efficace (2 parcours de liste)
(define (dedoublonne_1 L)
  (cond ((null? L) L)                     
        ((null? (cdr L)) L)               
        ((equal? (car L) (cadr L)) (dedoublonne_1 (cdr L)))          
        (else (cons (car L) (dedoublonne_1 (cdr L))))))
(define (doublonne-exacte_2 L)
  (doublonne (dedoublonne_1 L)))

(display "------------------------------")(newline)
(display "tests fonction doublonne-exacte")(newline)
(display "------------------------------")(newline)
(print-and-eval '(doublonne-exacte '(a b c)))
(print-and-eval '(doublonne-exacte '(a a b c)))
(print-and-eval '(doublonne-exacte '(a b a)))
(print-and-eval '(doublonne-exacte_2 '(a b c)))
(print-and-eval '(doublonne-exacte_2 '(a a b c)))
(print-and-eval '(doublonne-exacte_2 '(a b a)))
(display "------------------------------")(newline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3) fonction prefixe
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (prefixe L1 L2)
  (cond ((null? L1) '())
        ((null? L2) '())
        ((equal? (car L1) (car L2)) (cons (car L1) (prefixe (cdr L1)(cdr L2))))
        (else '())))

(display "------------------------------")(newline)
(display "tests fonction prefixe")(newline)
(display "------------------------------")(newline)
(print-and-eval '(prefixe '(a b c) '(a b d)))
(print-and-eval '(prefixe '(a b c) '(a b)))
(print-and-eval '(prefixe '(a b) '(a b c)))
(print-and-eval '(prefixe '(a b) '(c d)))
(display "------------------------------")(newline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4) fonction prefixe
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (suffixe L1 L2)
  (reverse (prefixe (reverse L1) (reverse L2))))

(display "------------------------------")(newline)
(display "tests fonction suffixe")(newline)
(display "------------------------------")(newline)
(print-and-eval '(suffixe '(a b c) '(e b c)))
(print-and-eval '(suffixe '(a b c) '(b c)))
(print-and-eval '(suffixe '(a b c) '(a b f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 5) fonction triee?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (triee? L comp?)
  (cond ((null? L) #t)
        ((null? (cdr L)) #t)
        ((comp? (car L) (cadr L)) (triee? (cdr L) comp?))
        (else #f)))

(display "------------------------------")(newline)
(display "tests fonction triee?")(newline)
(display "------------------------------")(newline)
(print-and-eval '(triee? '(1 2 3) <))
(print-and-eval '(triee? '(1 2 3) >))
(print-and-eval '(triee? '(1 3 2) <))
(print-and-eval '(triee? '(1 2 2) <))
(print-and-eval '(triee? '(1 2 2) <=))
(print-and-eval '(triee? '("aa" "ab" "cd") string<?))
(display "------------------------------")(newline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 6) fonction aplatir
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (aplatir L)
  (cond ((null? L) L)
        ((list? (car L)) (append (aplatir (car L)) (aplatir (cdr L))))
        (else (cons (car L) (aplatir (cdr L))))))

(display "------------------------------")(newline)
(display "tests fonction aplatir")(newline)
(display "------------------------------")(newline)
(print-and-eval '(aplatir '(a b c)))
(print-and-eval '(aplatir '((a b c) d (e f (g h)) i)))
(display "------------------------------")(newline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7) fonction combine 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (combine L1 L2)
  (define (iter L1 P)
    (cond ((null? L1) '())
          ((null? P) (iter (cdr L1) L2))
          (else (cons (cons (car L1) (car P)) (iter L1 (cdr P))))))
  (iter L1 L2))

(display "------------------------------")(newline)
(display "tests fonction combine")(newline)
(display "------------------------------")(newline)
(print-and-eval '(combine '(a b c) '(1 2)))
(print-and-eval '(combine '(a b c) '()))
(print-and-eval '(combine '() '(1 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 8) fonction doublonne-n  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (doublonne-n L n)
  (define (iter i L)
    (cond ((null? L) L)
          ((zero? i) (iter n (cdr L)))
          (else (cons (car L) (iter (sub1 i) L)))))
  (iter n L))
(display "------------------------------")(newline)
(display "tests fonction doublonne-n")(newline)
(display "------------------------------")(newline)
(print-and-eval '(doublonne-n '(a b c) 3))
(print-and-eval '(doublonne-n '(a b c) 0))
(print-and-eval '(doublonne-n '(a a b c) 3))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 9) fonction doublonne-exacte-n  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (doublonne-exacte-n L n)
  (define (iter i L)
    (cond ((null? L) L)
          ((zero? i) (iter n (cdr L)))
          ((null? (cdr L)) (cons (car L)(iter (sub1 i) L)))
          ((equal? (car L) (cadr L)) (iter i (cdr L)))
          (else (cons (car L) (iter (sub1 i) L)))))
  (iter n L))
(display "------------------------------")(newline)
(display "tests fonction doublonne-exacte-n")(newline)
(display "------------------------------")(newline)
(print-and-eval '(doublonne-exacte-n '(a b c) 3))
(print-and-eval '(doublonne-exacte-n '(a b c) 0))
(print-and-eval '(doublonne-exacte-n '(a a a a b c) 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 10) fonction anagramme?  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (anagramme? L1 L2)
    (define (in-liste? e L)
    (cond ((null? L) #f)
          ((equal? e (car L)) #t)
          (else (in-liste? e (cdr L)))))
  (define (supprime e L)
    (cond ((null? L) L)
          ((equal? e (car L)) (supprime e (cdr L)))
          (else (cons (car L) (supprime e (cdr L))))))
  (cond ((and (null? L1) (null? L2)) #t)
        ((null? L1) #f)
        ((null? L2) #f)
        ((in-liste? (car L1) L2) (anagramme? (cdr L1) (supprime (car L1) L2)))
        (else #f)))

;; bonus : faire une fonction qui génére une liste de tous les anagrammes d'une liste

(display "------------------------------")(newline)
(display "tests fonction anagramme?")(newline)
(display "------------------------------")(newline)
(print-and-eval '(anagramme? '(n o e l) '(l e o n)))
(print-and-eval '(anagramme? '(n e o n) '(l e o n)))
        