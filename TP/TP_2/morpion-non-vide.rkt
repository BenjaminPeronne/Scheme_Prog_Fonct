#lang racket
; NOM : LAURETTA-PERONNE 
; PRENOM : Aymerick
; TP 2 - SCHEME 
; MORPION
; 
; L'objectif du TP est de realiser un Moprion (tic-tac-toe)
; en scheme a deux joueurs humains (pour l'IA on verra cela dans un
; prochain TP)

; Il est conseille de suivre le schema indique, en implementant les fonctions
; dans cet ordre et de les tester au fur et a mesure

; les etudiants rapides (et courageux) pourront s'inspirer 
; de cette implementation pour faire un 'puissance 4' ... 
; -------------------------------------------
; Constructeur du morpion
; On pourra utiliser le type vector de scheme
; (a.k.o tableau a la C)
; pour la representation interne du plateau
; Fonctions utiles :
;  (make-vector n obj) ; cree un vector de taille n, 
;                      ; initialisé avec obj 
; notez que pour representer une case vide on peut utiliser
; le symbole * (pensez a le quoter !)
(define (make-morpion)
  (make-vector 9 '*)) 

; test de la fonction :
;;; (make-morpion)
; -------------------------------------------

; -------------------------------------------
; Renvoie le symbole present dans  
; la case (x,y) du plateau
; x et y sont donnes en coordonnees de 1 a 3
; Fonction utile :
;  (vector-ref v k) ; renvoie le symbole en position k du vector v
;                   ; les positions sont numérotes de 0 a n-1       
(define (2D->1D x y)
  (let ((a 0))
  (cond
    [(and (equal? y 2) (not(equal? x 3)))
     (set! a (quotient 2 x))]
    [(and (equal? y 3) (not (equal? x 3)))
     (set! a (* (quotient 2 x) 2))]
    [else (* x y)]
  )
    (+ (* x y) (- a 1)))
)

(define (get-case-morpion plateau x y)
  (vector-ref plateau (2D->1D x y))
)

; test de la fonction :
;;; (get-case-morpion (make-morpion) 1 1)

; -------------------------------------------

; -------------------------------------------
; Affiche le plateau
; Fct utile :
; (display obj) ; affiche l'objet obj
; (newline) ; retour à la ligne

(define (print-morpion plateau)
  (define flag 0)
  (define (aux x)
    (cond
      [(and (not(zero? x)) (zero?(modulo x 3)))
       (newline)]
      [else 0])
    (cond
      [(<= x 8)
      (display (vector-ref plateau x))
      (display " ")
      (aux (+ x 1))]
      [else (set! flag 1)]))
    
  (if (zero? flag)
      (aux 0)
      (0))
)


; test de la fonction :
;;; (print-morpion (make-morpion))
; -------------------------------------------

; -------------------------------------------
; Marque le coup du joueur player (represente par le symbole X
; ou O (pensez a quoter) a la position (x,y) 
; Fct utile :
;   (vector-set! v k obj) ; copie obj a la position
;                         ; k du vector v 
(define (set-case-morpion plateau player x y)
  (vector-set! plateau (2D->1D x y) player)
  plateau)

  
; test de la fonction :
;;; (print-morpion (set-case-morpion (make-morpion) 'X 1 1)) 
;;; (print-morpion (set-case-morpion (make-morpion) 'O 3 3)) 
;;; (print-morpion (set-case-morpion (make-morpion) 'O 1 3)) 
; -------------------------------------------

; -------------------------------------------
; renvoie #t si le coup est legal (case (x,y) valide et vide
(define (legal-move-morpion? plateau x y)
  (if (equal? (get-case-morpion plateau x y) '*) #t #f)
  )

; test de la fonction :
;;; (legal-move-morpion? (make-morpion) 1 1)
;;; (legal-move-morpion? (set-case-morpion (make-morpion) 'X 1 1) 1 1)
; -------------------------------------------

; -------------------------------------------
; renvoie #t si une position gagnante est reconnue
; alignement de 3 cases identiques et differente de *
;;;  11 22 33
;;;  11 12 13
;;;  21 22 23
;;;  31 32 33
;;;  33 22 11
;;;  11 21 31
;;;  12 22 32
;;;  13 23 33

;;;   1  2  3
;;; 1 *  *  *
;;; 2 *  *  *
;;; 3 *  *  *
   
(define (winning-morpion? plateau)
  (define (aux player)
    (or
     (and (eq? (get-case-morpion plateau 1 1) player)(eq? (get-case-morpion plateau 1 2) player)(eq? (get-case-morpion plateau 1 3) player))
     (and (eq? (get-case-morpion plateau 2 1) player)(eq? (get-case-morpion plateau 2 2) player)(eq? (get-case-morpion plateau 2 3) player))
     (and (eq? (get-case-morpion plateau 3 1) player)(eq? (get-case-morpion plateau 3 2) player)(eq? (get-case-morpion plateau 3 3) player))

     (and (eq? (get-case-morpion plateau 1 1) player)(eq? (get-case-morpion plateau 2 1) player)(eq? (get-case-morpion plateau 3 1) player))
     (and (eq? (get-case-morpion plateau 1 2) player)(eq? (get-case-morpion plateau 2 2) player)(eq? (get-case-morpion plateau 3 2) player))
     (and (eq? (get-case-morpion plateau 1 3) player)(eq? (get-case-morpion plateau 2 3) player)(eq? (get-case-morpion plateau 3 3) player))

     (and (eq? (get-case-morpion plateau 1 1) player)(eq? (get-case-morpion plateau 2 2) player)(eq? (get-case-morpion plateau 3 3) player))
     (and (eq? (get-case-morpion plateau 1 3) player)(eq? (get-case-morpion plateau 2 2) player)(eq? (get-case-morpion plateau 3 1) player))
     )
    )
  (or (aux 'X) (aux 'O))
  )


; test de la fonction :
;;; (winning-morpion?   (make-morpion))  
;;; (let* ((p (make-morpion))(p (set-case-morpion p 'X 1 1))(p (set-case-morpion p 'X 1 2))(p (set-case-morpion p 'X 1 3))) (print-morpion p) (winning-morpion? p))
;;; (let* ((p (make-morpion))(p (set-case-morpion p 'X 2 1))(p (set-case-morpion p 'X 2 2))(p (set-case-morpion p 'X 2 3))) (print-morpion p) (winning-morpion? p))
;;; (let* ((p (make-morpion))(p (set-case-morpion p 'X 3 1))(p (set-case-morpion p 'X 3 2))(p (set-case-morpion p 'X 3 3))) (print-morpion p) (winning-morpion? p))
;;; (let* ((p (make-morpion))(p (set-case-morpion p 'X 1 1))(p (set-case-morpion p 'X 2 1))(p (set-case-morpion p 'X 3 1))) (print-morpion p) (winning-morpion? p))
;;; (let* ((p (make-morpion))(p (set-case-morpion p 'X 1 2))(p (set-case-morpion p 'X 2 2))(p (set-case-morpion p 'X 3 2))) (print-morpion p) (winning-morpion? p))
;;; (let* ((p (make-morpion))(p (set-case-morpion p 'X 1 3))(p (set-case-morpion p 'X 2 3))(p (set-case-morpion p 'X 3 3))) (print-morpion p) (winning-morpion? p))
;;; (let* ((p (make-morpion))(p (set-case-morpion p 'X 1 1))(p (set-case-morpion p 'X 2 2))(p (set-case-morpion p 'X 3 3))) (print-morpion p) (winning-morpion? p))
;;; (let* ((p (make-morpion))(p (set-case-morpion p 'X 1 3))(p (set-case-morpion p 'X 2 2))(p (set-case-morpion p 'X 3 1))) (print-morpion p) (winning-morpion? p))
;;; (let* ((p (make-morpion))(p (set-case-morpion p 'X 1 3))(p (set-case-morpion p 'O 2 2))(p (set-case-morpion p 'X 3 1))) (print-morpion p) (winning-morpion? p))
;;; (let* ((p (make-morpion))(p (set-case-morpion p 'X 1 3))(p (set-case-morpion p 'X 3 1))) (print-morpion p) (winning-morpion? p))
;;; (let* ((p (make-morpion))(p (set-case-morpion p 'O 1 3))(p (set-case-morpion p 'O 2 2))(p (set-case-morpion p 'O 3 1))) (print-morpion p) (winning-morpion? p))

; notez l'utilisation de let* pour simuler plusieurs coups, expliquez pourquoi let*
; -------------------------------------------

; -------------------------------------------
; lit un coup au clavier pour le joueur player (X ou O)
; recommence si le coup n'est pas legal
; Fct utile :
; (read) ; lit au clavier et renvoie la valuer lue

;;; (define (move-morpion plateau player 'O) 'X))
;;;   (get-case-morpion plateau 'x 'y)
;;;   (read 'x)
;;;   (read 'y)
;;; )

(define (move-morpion plateau player)
    ((let ((x (read)) (y (read))) (legal-move-morpion? plateau x y) )))

; test de la fonction :
;;; (print-morpion (move-morpion (make-morpion) 'X))
;;; (print-morpion (move-morpion (set-case-morpion (make-morpion) 'O 1 3) 'X))
; -------------------------------------------

; -------------------------------------------
; renvoie X si player est O et reciproquement
(define (exchange player)
  (cond
    [(equal? player 'O) 'X]
    [else 'O]
  ))
; -------------------------------------------

; -------------------------------------------
; renvoie #t si toutes les cases sont occupees
; #f sinon
(define (end-morpion? plateau x y)
  (if (equal? (get-case-morpion plateau x y) 'X 'O) #t #f)
  )
; -------------------------------------------

; -------------------------------------------
; fonction principale
(define (play-morpion)
  ('vide))
  
