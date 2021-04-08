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
;; 1) fonction ackermann
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ack m n)
  (cond ((zero? m) (add1 n))
        ((zero? n) (ack (sub1 m) 1))
        (else (ack (sub1 m) (ack m (sub1 n))))))

(display "------------------------------")(newline)
(display "tests fonction ackerman")(newline)
(display "------------------------------")(newline)
(print-and-eval '(ack 1 1))
(print-and-eval '(ack 2 2))
(print-and-eval '(ack 3 3))
;; cette fonction croit très vite
;; ce qu'on peut voir sur la progression de (ack n n)
;; de n = 1 à 3
;; (cf https://fr.wikipedia.org/wiki/Fonction_d%27Ackermann )
;; par exemple  (ack 4 4) est très très très grand
(display "------------------------------")(newline)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2) somme-pos
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (somme-pos n m)
  (let ((s (+ n m)))
    (cond ((> s 0) s)
          (else 0))
    )
  )
(display "------------------------------")(newline)
(display "tests somme-pos")(newline)
(display "------------------------------")(newline)
(print-and-eval '(somme-pos 2 3))
(print-and-eval '(somme-pos 1 -3))
;; La programmation fonctionnelle n'interdit pas les variables locales :
;; l'utilisation du let permet de ne calculer qu'une seule fois la somme
;; qui est donnée à la variable locale s
;; la variable s est connue dasn l'évaluation du let
(display "------------------------------")(newline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3) somme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (somme a b)
  (cond ((zero? b) a)
        (else (somme (add1 a) (sub1 b)))))

(display "------------------------------")(newline)
(display "tests somme")(newline)
(display "------------------------------")(newline)
(print-and-eval '(somme 4 5))
(print-and-eval '(somme 10 6))
;; cela ne fonctionne que pour les entiers naturels
;; si b est négatif, la récursivité ne s'arrete pas !
;; bonus : somme-gen : génére une fonction d'addition a deux paramètres
;; selon les trois opérateurs :
;; - predecesseur
;; - successeur
;; - z?
(define (somme-gen predecesseur successseur z?)
  (letrec ((f (lambda (a b)
      (cond ((z? b) a)
        (else (f (successseur a) (predecesseur b)))))))
           f))
;; exemple d'utilisation
(print-and-eval '((somme-gen sub1 add1 zero?) 4 5))
(print-and-eval-ln '((somme-gen cdr (lambda (l) (cons 1 l)) null?) '(1 1 1 1) '(1 1 1 1 1)))
(display "------------------------------")(newline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4)  Fonctions mystère
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (f1 n)
(or (= n 0) (f2 (- n 1))))
(define (f2 n)
(and (not (= n 0)) (f1 (- n 1))))

(display "------------------------------")(newline)
(display "tests fcts mystere")(newline)
(display "------------------------------")(newline)

(print-and-eval '(f1 6))
(print-and-eval '(f1 3))
(print-and-eval '(map f1 '(0 1 2 3 4 5 6 7 8 9)))
(print-and-eval '(map f2 '(0 1 2 3 4 5 6 7 8 9)))
;; il s'agit des fonction even? et odd?
;; encore une fois, on ne pourrait les définir qu'avec add1 sub1 et zero?
;; ou plus génériquement successeur predecesseur et z?
(display "------------------------------")(newline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 5 et 6)  Fonction dedoublonne
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; une première version qui n'enleve que les doublons consécutifs 
(define (dedoublonne_1 L)
  (cond ((null? L) L)                      ; la liste est vide -> '()
        ((null? (cdr L)) L)                ; la liste ne contient qu'un elt -> L
        ((equal? (car L) (cadr L))         ; les 2 premiers elts sont egaux
         (dedoublonne_1 (cdr L)))          ; on ne traite pas le premier 
        (else                              ; sinon on ajoute le 1er elt au resultat de
         (cons (car L) (dedoublonne_1 (cdr L)))))) ; l'appel recursif sur le reste
;; une deuxième version qui ne conserve pas l'ordre (le premier doublon est supprimé
(define (dedoublonne_2 L)
  (define (in-liste? e L)
    (cond ((null? L) #f)
          ((equal? e (car L)) #t)
          (else (in-liste? e (cdr L)))))
  (cond ((null? L) L)                               
        ((in-liste? (car L) (cdr L)) (dedoublonne_2 (cdr L)))        
        (else (cons (car L) (dedoublonne_2 (cdr L)))))) 
;; une troisieme version qui conserve l'ordre
(define (dedoublonne_3 L)
  (define (in-liste? e L)
    (cond ((null? L) #f)
          ((equal? e (car L)) #t)
          (else (in-liste? e (cdr L)))))
  (define (supprime e L)
    (cond ((null? L) L)
          ((equal? e (car L)) (supprime e (cdr L)))
          (else (cons (car L) (supprime e (cdr L))))))
  (cond ((null? L) L)                               
        ((in-liste? (car L) (cdr L)) (cons (car L) (dedoublonne_3 (supprime (car L) (cdr L)))))      
        (else (cons (car L) (dedoublonne_3 (cdr L)))))) 
;; une quatrième version plus rapide
(define (dedoublonne_4 L)
  (define (supprime e L)
    (cond ((null? L) L)
          ((equal? e (car L)) (supprime e (cdr L)))
          (else (cons (car L) (supprime e (cdr L))))))
  (cond ((null? L) L)                                    
        (else (cons (car L) (dedoublonne_4 (supprime (car L) (cdr L)))))))

(display "------------------------------")(newline)
(display "tests fcts dedoublonne")(newline)
(display "------------------------------")(newline)

(print-and-eval '(dedoublonne_1 '(a a a b b a a c c c)))
(print-and-eval '(dedoublonne_2 '(a f b d c d a e f)))
(print-and-eval '(dedoublonne_3 '(a f b d c d a e f)))
(print-and-eval '(dedoublonne_4 '(a f b d c d a e f)))
(display "------------------------------")(newline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7)  Fonction tsd
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (tsd L)
  (cond ((null? L) L)
        ((null? (cdr L)) '())
        (else (cons (car L) (tsd (cdr L))))))

;; ou alors (moins efficace mais plus simple à écrire)
(define (tsd_2 L) (reverse (cdr (reverse L))))

(display "------------------------------")(newline)
(display "test tsd")(newline)
(display "------------------------------")(newline)

(print-and-eval '(tsd '(a b c d e f)))
(print-and-eval '(tsd_2 '(a b c d e f)))
(display "------------------------------")(newline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 8)  Fonction tous-egaux
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (tous-egaux? L)
  (cond ((null? L) #t)
        ((null? (cdr L)) #t)
        ((eq? (car L) (cadr L)) (tous-egaux_2? (cdr L)))
        (else #f)))
;; on pourrait faire sans le cond (plus élégant, mais plus abscons ...)
(define (tous-egaux_2? L)
  (or (null? L)
      (null? (cdr L))
      (and (eq? (car L) (cadr L)) (tous-egaux_2? (cdr L)))))

(display "------------------------------")(newline)
(display "test tous-egaux")(newline)
(display "------------------------------")(newline)

(print-and-eval '(tous-egaux? '(a s d f g)))
(print-and-eval '(tous-egaux? '(r r r r a)))
(print-and-eval '(tous-egaux? '(r r r r r)))
(print-and-eval '(tous-egaux_2? '(a s d f g)))
(print-and-eval '(tous-egaux_2? '(r r r r a)))
(print-and-eval '(tous-egaux_2? '(r r r r r)))
(display "------------------------------")(newline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 8)  Fonction pyra
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (pyra n)
  (define (iter i e) ;; affiche i fois la valeur de e
    (cond ((zero? i)(newline) )
          (else (display e)(iter (sub1 i) e))))
  (define (iter2 j) ;; iteration de 0 a n
    (cond ((> j n) (void) ) ;; la fonction (void) renvoie rien
          (else (iter (add1 j) j) (iter2 (add1 j)))))
  (iter2  0))
;; on peut regrouper les deux itérations
(define (pyrabis n)
  (define (iter i j)
    (cond ((> j n) (void)) ;; c'est fini on ne fait rien
          ((> i j) (newline)(iter 0 (add1 j))) ;; fin de ligne, on passe à la suivante 
          (else (display j)(iter (add1 i) j))))  
  (iter 0 0))
;; version de la question 10
(define (pyra_2 n)
  (define (iter i j)
    (cond ((< j 0) (void)) ;; c'est fini on ne fait rien
          ((> i n) (newline)(iter 0 (sub1 j))) ;; fin de ligne, on passe à la suivante 
          ((< i j) (display " ")(iter (add1 i) j))
          (else (display j)(iter (add1 i) j))))  
  (iter 0 n))

;; une autre version
(define (pyra_3 n)
  (define (iter i)
    (cond ((zero? i) )
          (else (display n)(iter (sub1 i)))))
  (cond ((zero? n) (iter 1)(newline))
        (else (iter (add1 n)) (newline) (pyra_3 (sub1 n)))))

;; il y a 8 pyramides différentes saurez vous trouver les 5 autres ?

(print-and-eval-ln '(pyra 9))
(print-and-eval-ln '(pyrabis 9))
(print-and-eval-ln '(pyra_2 9))
(print-and-eval-ln '(pyra_3 9))