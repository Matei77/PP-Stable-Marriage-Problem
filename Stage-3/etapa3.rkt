; Ionescu Matei-Stefan - 323CAb - 2022-2023

#lang racket

(require "etapa2.rkt")

(provide (all-defined-out))

; TODO 1
; După modelul funcției stable-match?, implementați funcția
; get-unstable-couples care primește o listă de logodne
; engagements, o listă de preferințe masculine mpref și o 
; listă de preferințe feminine wpref, și întoarce lista
; tuturor cuplurilor instabile din engagements.
; Precizări (aspecte care se garantează, nu trebuie verificate):
; - fiecare cuplu din lista engagements are pe prima poziție
;   o femeie
; Nu este permisă recursivitatea pe stivă.
; Nu sunt permise alte funcții ajutătoare decât
; better-match-exists? și funcțiile de manipulare a listelor de
; preferințe definite în etapele anterioare.
; Nu este permisă apelarea multiplă a aceleiași funcții pe
; aceleași argumente.
; Folosiți una sau mai multe dintre expresiile let, let*, letrec,
; named let pentru a vă putea conforma acestor restricții.
(define (get-unstable-couples engagements mpref wpref)
  (let ((rev-eng (map (lambda (x) (cons (cdr x) (car x))) engagements))) ; rev-eng = lista logodnelor cu personele inversate
    
    (let iter ((L engagements) ; L = lista logodnelor
               (result null))  ; result = lista cuplurilor instabile pe care o formam

      (if (null? L)
          result ; am verificat toate logodnele, deci returnam lista cuplurilor instabile        
          (let* ((current-eng (car L))     ; current-eng = logodna pe care o verificam in acest pas
                 (remaining-eng (cdr L))   ; remaining-eng = lista de logodne pe care nu le-am verificat inca
                 (w (car current-eng))     ; w = femeia din logodna curenta
                 (m (cdr current-eng)))    ; m = barbatul din logodna curenta
       
            (if (or (better-match-exists? m w (get-pref-list mpref m) wpref engagements)
                    (better-match-exists? w m (get-pref-list wpref w) mpref rev-eng))

                ; daca logodna este instabila o adaugam la rezultat si verificam restul listei de logodne
                (iter remaining-eng (cons current-eng result))

                ; altfel verificam restul listei de logodne
                (iter remaining-eng result)))))))
      


; TODO 2
; Implementați funcția engage care primește o listă free-men
; de bărbați încă nelogodiți, o listă de logodne parțiale 
; engagements (unde fiecare cuplu are pe prima poziție o femeie),
; o listă de preferințe masculine mpref și o listă de preferințe 
; feminine wpref, și întoarce o listă completă de logodne stabile,
; obținută conform algoritmului Gale-Shapley:
; - cât timp există un bărbat m încă nelogodit
;   - w = prima femeie din preferințele lui m pe care m nu a cerut-o încă
;   - dacă w este nelogodită, adaugă perechea (w, m) la engagements
;   - dacă w este logodită cu m'
;     - dacă w îl preferă pe m lui m'
;       - m' devine liber
;       - actualizează partenerul lui w la m în engagements
;     - altfel, repetă procesul cu următoarea femeie din lista lui m
; Folosiți named let pentru orice proces recursiv ajutător (deci nu
; veți defini funcții ajutătoare recursive).
; Folosiți let și/sau let* pentru a evita calcule duplicate.
(define (engage free-men engagements mpref wpref)
  (let iter ((L free-men)       ; L = lista barbatilor nelogoditi
             (eng engagements)) ; eng = lista de logodne
    
    (if (null? L)
        eng ; niciun barbat nelogodit ramas, deci returnam lista de logodne stabile
        (let mpref-iter ((m-pref-list (get-pref-list mpref (car L)))) ; m-pref-list = lista de preferinte a lui m
    
          (let* ((m (car L))                           ; m = primul barbat din lista de barbati nelogoditi
                 (w (car m-pref-list))                 ; w = prima femeie din lista de preferinte a lui m
                 (w-pref-list (get-pref-list wpref w)) ; w-pref-list = lista de preferinte a lui w
                 (l (get-partner eng w)))              ; l = logodnicul actual al lui w
        
            (cond
              ; w este nelogodita
              ((not l) (iter (cdr L) (cons (cons w m) eng)))
        
              ; w il prefera pe m fata de l
              ((preferable? w-pref-list m l) (iter (cons l (cdr L)) (update-engagements eng w m)))

              ; altfel repeta cu urmatoarea femeie din m-pref-list
              (else (mpref-iter (cdr m-pref-list)))))))))
              


; TODO 3
; Implementați funcția gale-shapley care este un wrapper pentru
; algoritmul implementat de funcția engage. Funcția gale-shapley
; primește doar o listă de preferințe masculine mpref și o listă
; de preferințe feminine wpref și calculează o listă completă de
; logodne stabile conform acestor preferințe.
(define (gale-shapley mpref wpref)
  (engage (get-men mpref) null mpref wpref))


; TODO 4
; Implementați funcția get-couple-members care primește o listă
; de perechi cu punct și întoarce o listă simplă cu toate elementele 
; care apar în perechi.
; Folosiți funcționale, fără recursivitate explicită.
(define (get-couple-members pair-list)
  (foldr (lambda (x acc) (cons (cdr x) (cons (car x) acc))) null pair-list)) 

