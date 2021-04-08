;;; 
;;;   @author Benjamin Peronne
;;;   @email contact@benjaminperonne.fr
;;;   @create date 2021-02-23 12:34:35
;;;   @modify date 2021-02-23 12:34:35
;;;   @desc [TP_5]
;;;  

#lang racket
(require racket/gui/base)

(define repertoire '())

(define (ajout-r rep nom prenom)
  (if (null? rep) 
      (cons (list 0 nom prenom) rep) 
      (cons (list (add1 (caar rep)) nom prenom) rep)))

(set!  repertoire (ajout-r repertoire "dupond" "jean"))
(set!  repertoire (ajout-r repertoire "dupond" "jacques"))
(set!  repertoire (ajout-r repertoire "duval" "jean"))
(set!  repertoire (ajout-r repertoire "dupres" "jacques"))

repertoire

(define (affiche-r rep)
  (cond ((null? rep) (display ""))
        (else (display (caar rep))(display " : ") (display (cadar rep))(display " -- ") (display (caddar rep)) (newline) (affiche-r (cdr rep)))))

(affiche-r repertoire)

(define (tri-ins L pred?)
  (define (insere x L)
    (cond ((null? L) (cons x L))
          ((pred? x (car L)) (cons (car L) (insere x (cdr L))))
          (else (cons x L))))
    (if (null? L)
        L
        (insere (car L) (tri-ins (cdr L) pred?))))

(define (compare-p p1 p2)
  (or (string>? (cadr p1) (cadr p2)) 
      (and (string=? (cadr p1) (cadr p2))
           (string>? (caddr p1) (caddr p2)))))

(define (affiche-tri-r rep)
  (affiche-r (tri-ins rep compare-p)))

(set!  repertoire (ajout-r repertoire "aa" "aa"))
(set!  repertoire (ajout-r repertoire "aa" "zz"))
(set!  repertoire (ajout-r repertoire "zz" "aa"))
(set!  repertoire (ajout-r repertoire "zz" "zz"))

"repertoire non trié"
(affiche-r repertoire)

"repertoire trié"
(affiche-tri-r repertoire)

;suppression
(define (supprime-r rep id)
  (if (= id (caar rep))
      (cdr rep)
      (cons (car rep) (supprime-r (cdr rep) id))))

"suppression de 0, 7 et 3"
(set!  repertoire (supprime-r repertoire 0))
(set!  repertoire (supprime-r repertoire 7))
(set!  repertoire (supprime-r repertoire 3))

"repertoire non trié"
(affiche-r repertoire)

(define (extract rep lettre)
  (cond ((null? rep) '())
        ((eq? lettre (string-ref (cadar rep) 0)) (cons (car rep) (extract (cdr rep) lettre)))
        (else (extract (cdr rep) lettre))))

"repertoire commence par d"
(define repertoire-d (extract repertoire #\d))
(affiche-r repertoire-d)

"repertoire commence par a"
(define repertoire-a (extract repertoire #\a))
(affiche-r repertoire-a)

"sauvegarde dans un fichier"
(define (save-r rep)
  (define out (open-output-file "repertoire.txt" #:exists 'replace ))
  (write rep out)
  (close-output-port out))

"lecture dans un fichier"
(define (load-r)
  (define in (open-input-file "repertoire.txt"))
  (read in))

(define (application)
  (define repertoire '())
  (define (menu)
    (display "0-quitter")(newline)
    (display "1-afficher")(newline)
    (display "2-ajouter")(newline)
    (display "3-sauver")(newline)
    (display "4-charger")(newline)
    (display "...")(newline)
    (let ((choix (read)))
      (cond ((eq? choix 0) (display "bye \n"))
            ((eq? choix 1) (affiche-r repertoire) (menu))
            ((eq? choix 2) (set! repertoire (ajout-r repertoire (read) (read))) (menu))
            (else (display " pas encore implémenté\n") (menu)))))
    (menu))

;Interface graphique
(define $repertoire
  (new frame% [label "Repertoire"][parent #f][width 300][height 500]))
(define $panel-v (new vertical-panel% [parent $repertoire]))
(define $panel-h (new horizontal-panel% [parent $panel-v]))

;////////////////////////////Affiche répertoire////////////////
(define (aff-rep rep)
  (let (($panel-ho (new horizontal-panel% [parent $panel-v])))
    (send $panel-ho set-alignment 'left 'top)
  (define $bouton-supp
  (new button% [label "delete"] [parent $panel-ho]
       [callback
        (lambda (b e)
          (supp (car rep))
          (affiche-r repertoire)
          (delete))
        ]
       ))
  (cond ((null? rep) '())
        ((null? (cdr rep)) (send $bouton-supp show #t)(new message% [label (string-append (number->string (caar rep)) " " (cadar rep) " " (caddar rep) "\n")] [parent $panel-ho]))
        (else (send $bouton-supp show #t)(new message% [label (string-append (number->string (caar rep)) " " (cadar rep) " " (caddar rep) "\n")] [parent $panel-ho])(aff-rep (cdr rep))))))
        

;///////////////BOUTTON AJOUT///////////////////////////
(define $nom "le nom avant")
(define $pnom "le prénom après")

(define $dialog
  (new dialog% [label "Ajoutation"][parent $repertoire][width 200][height 100]))

(define $texte-n
  (new text-field% [label "Votre Nom :"][parent $dialog][init-value "NOM"]))
(define $texte-p
  (new text-field% [label "Votre Prenom :"][parent $dialog][init-value "PRENOM"]))

(define $panel2 (new horizontal-panel% [parent $dialog]))
(send $panel2 set-alignment 'center 'center)
(define $bouton-ok
  (new button% [label "ok"][parent $panel2]
       [callback
        (lambda (b e)
          (let ((nom (send $texte-n get-value)) (pnom (send $texte-p get-value)))
            (set! $nom nom)
            (set! $pnom pnom)
            (set!  repertoire (ajout-r repertoire nom pnom))
            (send $dialog show #f)
            (delete)))
        ]
       ))

(define $bouton-cancel
  (new button% [label "annuler"][parent $panel2]
       [callback
        (lambda (b e)
          (send $dialog show #f))
        ]
       ))
;//////////////////////////////////////////////////////////////
;/////////////BOUTTON AJOUT////////////////////////
(define $bouton-ajout
  (new button% [label "ajouter"] [parent $repertoire]
       [callback
        (lambda (b e)
          (send $dialog show #t))
        ]
       ))
;///////////////////////////////////////////

;///////////////////SUPPP////////////////////////
(define (supp rep)
  (set!  repertoire (supprime-r repertoire (car rep))))
;//////////////////////////////////////////////


;//////////////////////DELETE/////////////////////////
(define (delete)
  (define (delete1)
    (send $panel-v delete-child (car (send $panel-v get-children)))(delete))
  (cond ((not (null? (send $panel-v get-children))) (delete1))
        (else (aff-rep repertoire))))
;////////////////////////////////////////////////////

(aff-rep repertoire)


(define (go)
  (send $repertoire show #t))
(go)
