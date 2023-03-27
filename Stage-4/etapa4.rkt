; Ionescu Matei-Stefan - 323CAb - 2022-2023

#lang racket

(require "etapa2.rkt")
(require "etapa3.rkt")

(provide (all-defined-out))

;; Preferințele bărbaților și femeilor din problemă se pot schimba
;; în timp, dar de obicei ele nu se schimbă radical de la un moment
;; la altul. De aceea, în loc să rulăm de la zero algoritmul
;; Gale-Shapley de fiecare dată când se schimbă ceva, preferăm să
;; pornim de la lista de logodne stabile obținută în pasul anterior
;; și să o actualizăm, conform algoritmului următor:
;; - eliminăm din engagements cuplurile care au devenit instabile
;;   în urma modificărilor de preferințe
;;   - cuplurile rămase sunt stabile între ele și considerăm că
;;     se găsesc împreună într-o cameră, în timp ce membrii cuplurilor
;;     destrămate stau la coadă la intrarea în cameră
;; - cât timp coada nu este goală
;;   - prima persoană p din coadă intră în cameră și încearcă să se
;;     cupleze cu cineva care este deja acolo, astfel:
;;     - p-list = lista de preferințe a lui p
;;     - determină prima persoană p' din p-list care este în cameră
;;     - dacă p' nu e logodită, logodește p' cu p
;;     - dacă p' e logodită
;;       - dacă p' îl preferă pe p partenerului actual p''
;;         - logodește p' cu p
;;         - încearcă să îl cuplezi pe p'' cu altcineva din cameră
;;           (folosind același algoritm)
;;       - altfel, treci la următoarea persoană din p-list (dacă
;;         aceasta există, altfel p rămâne temporar fără partener)


; TODO 1
; Implementați funcția match care primește o persoană person care
; intră în cameră, lista engagements a cuplurilor din cameră
; (cuplurile având pe prima poziție persoanele de gen opus lui 
; person), o listă pref1 care conține preferințele celor de același 
; gen cu person, o listă pref2 cu preferințele celor de gen diferit, 
; respectiv o coadă queue a persoanelor din afara camerei,
; și întoarce lista de cupluri actualizată astfel încât noile
; cupluri să fie stabile între ele.
; Această listă se obține ca rezultat al încercării de a cupla pe
; person cu cineva din cameră (person va încerca în ordine persoanele 
; din lista sa de preferințe), care poate duce la destrămarea
; unui cuplu și necesitatea de a cupla noua persoană rămasă singură
; cu altcineva din cameră, etc. Procesul continuă până când:
; - ori avem numai cupluri stabile între ele în cameră, nimeni
;   nefiind singur
; - ori toate persoanele rămase singure nu ar fi preferate de nimeni
;   altcineva din cameră, și în acest caz convenim să "logodim"
;   aceste persoane cu valoarea #f, astfel încât funcția să
;   întoarcă în aceeași listă atât informația despre cine din
;   cameră este logodit, cât și despre cine este singur
(define (match person engagements pref1 pref2 queue)
  (let pref-iter ((p-pref-list (get-pref-list pref1 person))) ; p-pref-list = lista de preferinte a lui person

    (if (null? p-pref-list)
        (cons (cons #f person) engagements) ; person nu s-a logodit cu nicio persona din camera
        
        (let* ((current-pref (car p-pref-list))                   ; current-pref = persoana curenta din lista de preferinte a lui person
               (l (get-partner engagements current-pref))         ; l = partenerul lui current-pref
               (cp-pref-list (get-pref-list pref2 current-pref))) ; cp-pref-list = lista de preferinte a lui current-pref
                     
          (cond
            ; current-pref nu este in camera asa ca trecem la urmatoarea persoana
            ((member current-pref queue) (pref-iter (cdr p-pref-list)))

            ; current-pref este in camera si nu are partener, deci person se logodeste cu current-pref
            ((not l) (update-engagements engagements current-pref person))

            ; current-pref il prefera pe person in detrimentul partenerului, deci acestia se logodesc si aplicam functia match pentru l care a ramas singur
            ((preferable? cp-pref-list person l) (match l (update-engagements engagements current-pref person) pref1 pref2 queue))

            ; current-pref nu il prefera pe person, deci trecem la urmatoarea persoana
            (else (pref-iter (cdr p-pref-list))))))))



; TODO 2
; Implementați funcția path-to-stability care primește lista
; engagements a cuplurilor din cameră, o listă de preferințe 
; masculine mpref, o listă de preferințe feminine wpref, respectiv
; coada queue a persoanelor din afara camerei, și întoarce lista
; completă de logodne stabile, obținută după ce fiecare persoană
; din queue este introdusă pe rând în cameră și supusă procesului
; descris de funcția match.
; Precizări (aspecte care se garantează, nu trebuie verificate):
; - fiecare cuplu din lista engagements are pe prima poziție
;   o femeie
; - persoanele nelogodite din cameră apar în engagements sub forma
;   (#f . nume-bărbat) sau (nume-femeie . #f)
(define (path-to-stability engagements mpref wpref queue)
  ; functie care interschimba barbatii si femeile din fiecare pereche
  (define (swap-pairs eng) (map (lambda (x) (cons (cdr x) (car x))) eng))

  (let iter ((q queue)          ; q = coada pentru intrarea in camera
             (eng engagements)) ; eng = lista de logodne

    (if (null? q)
        eng ; toate persoanele au intrat in camera si returnam lista de logodne
        (let* ((current-person (car q))) ; current-person = persoana care instra in acest moment in camera
          
          (if (member current-person (get-men mpref))
              ; current-person este barbat
              (iter (cdr q) (match current-person eng mpref wpref (cdr q)))
              
              ; current-person este femeie
              (iter (cdr q) (swap-pairs (match current-person (swap-pairs eng) wpref mpref (cdr q)))))))))
          


; TODO 3
; Implementați funcția update-stable-match care primește o listă 
; completă de logodne engagements (soluția anterioară), o listă de 
; preferințe masculine mpref și o listă de preferințe feminine wpref 
; (adică preferințele modificate față de cele pe baza cărora s-a 
; obținut soluția engagements), și calculează o nouă listă de logodne 
; stabile - conform cu noile preferințe, astfel:
; - unstable = cuplurile instabile din engagements
; - room-engagements = engagements - unstable
; - queue = persoanele din unstable
; - aplică algoritmul path-to-stability
; Precizări (aspecte care se garantează, nu trebuie verificate):
; - fiecare cuplu din lista engagements are pe prima poziție
;   o femeie
(define (update-stable-match engagements mpref wpref)
  (let* ((unstable-couples (get-unstable-couples engagements mpref wpref))                       ; unstable-couples = cuplurile instabile din engagements
         (queue (get-couple-members unstable-couples))                                           ; queue = persoanele din unstable-couples
         (room-engagements (filter (lambda (x) (not (member x unstable-couples))) engagements))) ; room-engagements = engagements - unstable-couples

    (path-to-stability room-engagements mpref wpref queue)))


; TODO 4
; Implementați funcția build-stable-matches-stream care primește
; un flux pref-stream de instanțe SMP și întoarce fluxul de 
; soluții SMP corespunzător acestor instanțe.
; O instanță SMP este o pereche cu punct între o listă de preferințe
; masculine și o listă de preferințe feminine.
; Fluxul rezultat se va obține în felul următor:
; - primul element se calculează prin aplicarea algoritmului
;   Gale-Shapley asupra primei instanțe
; - următoarele elemente se obțin prin actualizarea soluției
;   anterioare conform algoritmului implementat în etapa 4 a temei
; Trebuie să lucrați cu interfața pentru fluxuri. Dacă rezolvați
; problema folosind liste și doar convertiți în/din fluxuri,
; punctajul pe acest exercițiu se anulează în totalitate.
(define (build-stable-matches-stream pref-stream)
  (if (stream-empty? pref-stream)
      empty-stream ; daca streamul de preferinte este gol returnma empty-stream

      (let* ((first-prefs (stream-first pref-stream)) ; first-prefs = prima pereche de preferinte din pref-stream
             (first-mpref (car first-prefs))          ; first-mpref = prima lista de preferinte masculine
             (first-wpref (cdr first-prefs)))         ; first-wpref = prima lista de preferinte feminine
            
        (let loop ((eng (gale-shapley first-mpref first-wpref)) ; eng = lista de logodne din flux care este calculata cu Gale-Shapley pentru prima instanta
                   (prefs (stream-rest pref-stream)))           ; prefs = fluxul de instante SMP
        
          (if (stream-empty? prefs)
              (stream-cons eng empty-stream) ; am terminat instantele SMP si adaugam ultima lista de logodne la flux
            
              (let* ((current-prefs (stream-first prefs)) ; current-prefs = prechea de preferinte curenta din prefs
                     (current-mpref (car current-prefs))  ; current-mpref = lista de preferinte masculine curenta
                     (current-wpref (cdr current-prefs))) ; current-wpref = lista de preferinte feminine curenta

                ; adaugam lista de logodne curenta la flux
                (stream-cons eng (loop (update-stable-match eng current-mpref current-wpref) (stream-rest prefs)))))))))

