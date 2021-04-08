;;;   @author Benjamin Peronne
;;;   @email contact@benjaminperonne.fr
;;;   @create date 2021-01-26 11:15:22
;;;   @modify date 2021-02-16 11:17:13
;;;   @desc [TP_3 - Repertoire]"

#lang racket

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