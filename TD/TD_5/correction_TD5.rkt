#lang racket
(define-namespace-anchor anc)
(define ns (namespace-anchor->namespace anc))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definition du type abstrait arbre
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; constructeur d'arbre vide
(define (a-vide)
  '())

; constructeur d'arbre
(define (a-noeud v sag sad)
  (list v sag sad))

; predicat arbre
(define (a-noeud? a)
  (not (null? a)))

; predicat vide
(define (a-vide? a)
  (null? a))

; predicat feuille
(define (a-feuille? a)
  (and (a-noeud? a) 
       (a-vide? (a-gauche a)) 
       (a-vide? (a-droit a)))) 

; accesseur valeur
(define (a-valeur a)
  (car a))

; accesseur ss-arbre gauche
(define (a-gauche a)
  (cadr a))

; accesseur ss-arbre droit
(define (a-droit a)
  (caddr a))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; affichage d'un arbre
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;indentation
(define (blancs n)
  (cond  
    ((zero? n) (display ""))
    (else (display "\t") (blancs (- n 1)))))

; affichage prefixe indente
(define (a-print a)
  (define (aux a n)
    (cond ((a-vide? a) (blancs n) (display "()\n"))
          (else 
           (blancs n)
           (display (a-valeur a))
           (display "\n")
           (aux (a-gauche a) (+ n 1))
           (aux (a-droit a) (+ n 1)))))
  (aux a 0))

; affichage infixe indente
(define (a-print-inf a)
  (define (aux a n)
    (cond ((a-vide? a) (blancs n) (display "()\n"))
          (else 
           (aux (a-gauche a) (+ n 1))
           (blancs n)
           (display (a-valeur a))
           (display "\n")
           (aux (a-droit a) (+ n 1)))))
  (aux a 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parcours d'un arbre
;; transformation en liste
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;profondeur (gauche)
(define (parcours-prof a)
      (cond ((a-vide? a) '())
            (else (cons (a-valeur a) 
                        (append (parcours-prof (a-gauche a)) 
                                (parcours-prof (a-droit a)))))))

(define (parcours-prof-suf a)
  (cond ((a-vide? a) '())
        (else (append (parcours-prof (a-gauche a)) 
                      (parcours-prof (a-droit a))
                      (list (a-valeur a)) 
                      ))))
(define (parcours-prof-inf a)
  (cond ((a-vide? a) '())
        (else (append (parcours-prof (a-gauche a)) 
                      (list (a-valeur a)) 
                      (parcours-prof (a-droit a))
                      
                      ))))

(define AA (a-noeud 1 (a-noeud 2 (a-vide) (a-noeud 3 (a-vide) (a-vide) )) (a-noeud 4 (a-vide) (a-vide) )))
;largeur 
(define (parcours-largeur a)
  (define (aux L)
    (cond ((null? L) '())
          ((a-vide? (car L)) (aux (cdr L)))
          (else (cons (a-valeur (car L)) 
                      (aux (append (cdr L) 
                                   (list (a-gauche (car L)) 
                                         (a-droit (car L)))))))))
  (aux (list a)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; transformation liste en arbre 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; prefixe
(define (arbrifier L)
  (cond ((null? L) (a-vide))
        ((not (pair? L)) (a-noeud L (a-vide) (a-vide)))
        (else (a-noeud (car L) 
                       (arbrifier (cadr L)) 
                       (arbrifier (caddr L))))))
; infixe
(define (arbrifier-inf L)
  (cond ((null? L) (a-vide))
        ((not (pair? L)) (a-noeud L (a-vide) (a-vide)))
        (else (a-noeud (cadr L) 
                       (arbrifier-inf (car L)) 
                       (arbrifier-inf (caddr L))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; transformation arbre  en  liste
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; prefixe
(define (a-list a)
    (cond ((a-vide? a) '())
          ((a-feuille? a) (a-valeur a))
          (else 
           (list (a-valeur a) 
                 (a-list (a-gauche a)) 
                 (a-list (a-droit a))))))

; infixe
(define (a-list-inf a)
    (cond ((a-vide? a) '())
          ((a-feuille? a) (a-valeur a))
          (else 
           (list 
                 (a-list-inf (a-gauche a))
                 (a-valeur a) 
                 (a-list-inf (a-droit a))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; simplification d'arbre "arithmetiques"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (simplifier a)
    (cond ((a-vide? a) '())
          ((a-feuille? a) a)
          (else (let ((sag (simplifier (a-gauche a))) (sad (simplifier (a-droit a))))
                  (cond ((and (number? (a-valeur sag)) (number? (a-valeur sad))) 
                         (a-noeud 
                          (eval (list (a-valeur a) (a-valeur sag) (a-valeur sad)) ns) 
                          (a-vide) 
                          (a-vide)) )
                        ((number? sag) (a-noeud (a-valeur a) (a-noeud sag (a-vide) (a-vide)) sad))
                        ((number? sad) (a-noeud (a-valeur a) sag (a-noeud sad (a-vide) (a-vide))))
                        (else (a-noeud (a-valeur a) sag sad)))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TEST 
(display " ---- Definition d'un arbre ------\n")
(define A (a-noeud 100 
                   (a-noeud 2 (a-vide) 
                            (a-noeud 30 (a-vide) (a-vide) )) 
                   (a-noeud 4000(a-vide) (a-vide) )))

A
; -> (100 (2 () (30 () ())) (4000 () ()))
(display " ---- affichage d'un arbre ------\n")
(display " -> prefixe \n")
(a-print A)

; ->
;100
;	2
;		()
;		30
;			()
;			()
;	4000
;		()
;		()

(display " -> infixe \n")
(a-print-inf A)

; ->
;		()
;	2
;			()
;		30
;			()
;100
;		()
;	4000
;		()


(display " ---- parcours d'un arbre ------\n")
(display " -> profondeur \n")
(parcours-prof A)

; -> (100 2 30 4000)

(display " -> profondeur \n")
(parcours-largeur A)

; -> (100 2 4000 30)

(display " ---- liste -> arbre ------\n")
(display " -> prefixe \n")
(define L '(+ (- 3 4) (* 1 (+ 5 6))))
(display " liste : \n")
L

(define B (arbrifier L))
(a-print-inf B)
; -> 
;			()
;		3
;			()
;	-
;			()
;		4
;			()
;+
;			()
;		1
;			()
;	*
;				()
;			5
;				()
;		+
;				()
;			6
;				()
;

(display " -> infixe \n")
(define L2'((3 - 4) + (1 * (5 + 6))))
(display " liste : \n")
L2

(define C  (arbrifier-inf L2))
(a-print-inf C)

; -> 
;			()
;		3
;			()
;	-
;			()
;		4
;			()
;+
;			()
;		1
;			()
;	*
;				()
;			5
;				()
;		+
;				()
;			6
;				()
  
(display " ---- arbre -> liste ------\n")
(display " -> prefixe \n")
(define D (a-list C))
D

; ->  (+ (- 3 4) (* 1 (+ 5 6)))

(eval D ns)

; ->  10

(display " -> infixe \n")
(a-list-inf C)

; ->  ((3 - 4) + (1 * (5 + 6)))

(display " ---- simplifiaction d'arbre ------\n")

(display " -> exemple 1 \n")
(simplifier C)

; ->  (10 () ())

(display " -> exemple 2 \n")
(display " la liste :  \n")
(define L3 '((3 - 4) + (b * (5 + 6))))
L3
; ->  ((3 - 4) + (b * (5 + 6)))
(display " l'arbre :  \n")
(define E  (arbrifier-inf L3))
(a-print-inf E)
; -> 
;			()
;		3
;			()
;	-
;			()
;		4
;			()
;+
;			()
;		b
;			()
;	*
;				()
;			5
;				()
;		+
;				()
;			6
;				()

(display " l'arbre au format liste :  \n")
(a-list E)

; ->(+ (- 3 4) (* b (+ 5 6)))

(display " simplification de l'arbre :  \n")
(define F (simplifier E))
(a-print-inf F)

; ->
;		()
;	-1
;		()
;+
;			()
;		b
;			()
;	*
;			()
;		11
;			()

(display " au format liste :  \n")
(a-list F)

; -> (+ -1 (* b 11))

(display " au format liste infixe :  \n")
(a-list-inf F)

; -> (-1 + (b * 11))

(display " -> exemple 3 \n")

(a-list-inf 
  (simplifier 
   (arbrifier-inf '((3 - 4) + (b * ( (a * c) + (4 / 7)))))))
;->
