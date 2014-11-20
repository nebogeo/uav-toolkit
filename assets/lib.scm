;; Starwisp Copyright (C) 2013 Dave Griffiths
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.
;;
;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; debugging and unit tests

(define (msg . args)
  (for-each
   (lambda (i) (display i)(display " "))
   args)
  (newline))

(define (dbg i) (msg i) i)

(define (assert msg v)
  (display (string-append "testing " msg))(newline)
  (when (not v)
        (error "unit " msg)))

(define (asserteq msg a b)
  (display (string-append "testing " msg))(newline)
  (when (not (equal? a b))
        (error "unit " msg a b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list stuff

(define (filter fn l)
  (foldl
   (lambda (i r)
     (if (fn i) (append r (list i)) r))
   '()
   l))

(define (sort lst fn)
  (if (null? lst)
      '()
      (insert (car lst) fn
              (sort (cdr lst) fn))))

;; (chop (1 2 3 4) 2) -> ((1 2) (3 4))
(define (chop l n)
  (define (_ in out c)
    (display c)(newline)
    (cond
      ((null? in) out)
      ((zero? c) (_ (cdr in) (cons (list (car in)) out) (- n 1)))
      (else (_ (cdr in) (cons (cons (car in) (car out)) (cdr out)) (- c 1)))))
  (reverse (map reverse (_ l '(()) n))))

(define (crop l n)
  (cond
   ((null? l) '())
   ((zero? n) '())
   (else (cons (car l) (crop (cdr l) (- n 1))))))

;(define (in-list? n l)
;  (cond
;   ((null? l) #f)
;   ((equal? n (car l)) #t)
;   (else (in-list? n (cdr l)))))

(define in-list? string-in-list-fast) ;; optimisation

(define (find n l)
  (cond
    ((null? l) #f)
    ((equal? n (car (car l))) (car l))
    (else (find n (cdr l)))))

(define (findv n l)
  (cond
    ((null? l) #f)
    ((eqv? n (car (car l))) (car l))
    (else (findv n (cdr l)))))

;; find the index of an item in a flat list
(define (index-find n l)
  (define (_ l i)
    (cond
     ((null? l) #f)
     ((equal? n (car l)) i)
     (else (_ (cdr l) (+ i 1)))))
  (_ l 0))

(define (sorted-add l i)
  (cond
   ((null? l) (list i))
   ;; overwrite existing
   ((equal? (car i) (caar l)) (cons i (cdr l)))
   ((string<? (car i) (caar l))
    (cons i l))
   (else
    (cons (car l) (sorted-add (cdr l) i)))))

(define (sorted-find l k)
  (define (_ bot top)
    (if (null? l) #f
        (let* ((m (inexact->exact (floor (+ bot (/ (- top bot) 2)))))
               (mid (list-ref l m))
               (v (car mid)))
          (cond
           ((equal? k v) mid)
           ((eqv? top bot) #f)
           ((string<? k v) (_ bot m))
           (else (_ (+ m 1) top))))))
  (_ 0 (- (length l) 1)))

(define (sorted-addv l i)
  (cond
   ((null? l) (list i))
   ;; overwrite existing
   ((eqv? (car i) (caar l)) (cons i (cdr l)))
   ((< (car i) (caar l))
    (cons i l))
   (else
    (cons (car l) (sorted-addv (cdr l) i)))))

(define (sorted-findv l k)
  (define (_ bot top)
    (if (null? l) #f
        (let* ((m (inexact->exact (floor (+ bot (/ (- top bot) 2)))))
               (mid (list-ref l m))
               (v (car mid)))
          (cond
           ((eqv? k v) mid)
           ((eqv? top bot) #f)
           ((< k v) (_ bot m))
           (else (_ (+ m 1) top))))))
  (_ 0 (- (length l) 1)))

; utils funcs for using lists as sets
(define (set-remove a l)
  (cond
   ((null? l) '())
   (else
    (if (eqv? (car l) a)
        (set-remove a (cdr l))
        (cons (car l) (set-remove a (cdr l)))))))

(define (set-add a l)
  (if (not (memv a l))
      (cons a l) l))

(define (set-contains a l)
  (if (not (memq a l)) #f #t))


(define (build-list fn n)
  (define (_ fn n l)
    (cond ((zero? n) l)
          (else
           (_ fn (- n 1) (cons (fn (- n 1)) l)))))
  (_ fn n '()))

(define (foldl op initial seq)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op (car rest) result) (cdr rest))))
  (iter initial seq))

(define (insert-to i p l)
  (cond
   ((null? l) (list i))
   ((zero? p) (cons i l))
   (else
    (cons (car l) (insert-to i (- p 1) (cdr l))))))

;; (list-replace '(1 2 3 4) 2 100) => '(1 2 100 4)
(define (list-replace l i v)
  (cond
    ((null? l) l)
    ((zero? i) (cons v (list-replace (cdr l) (- i 1) v)))
    (else (cons (car l) (list-replace (cdr l) (- i 1) v)))))

(define (insert elt fn sorted-lst)
  (if (null? sorted-lst)
      (list elt)
      (if (fn elt (car sorted-lst))
          (cons elt sorted-lst)
          (cons (car sorted-lst)
                (insert elt fn (cdr sorted-lst))))))

(define (choose l)
  (list-ref l (random (length l))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; time

(define (time->seconds t)
   (+ (car t) (/ (cadr t) 1000000)))

(define start-time (time->seconds (time-of-day)))

(define (time-now)
  (- (time->seconds (time-of-day)) start-time))

;; just for graph so don't have to be accurate!!!
(define (date->day d)
  (+ (* (list-ref d 2) 360)
     (* (list-ref d 1) 30)
     (list-ref d 0)))

(define (date< a b)
  (cond
   ((< (list-ref a 2) (list-ref b 2)) #t)
   ((> (list-ref a 2) (list-ref b 2)) #f)
   (else ;; year is the same
    (cond
     ((< (list-ref a 1) (list-ref b 1)) #t)
     ((> (list-ref a 1) (list-ref b 1)) #f)
     (else ;; month is the same
      (cond
       ((< (list-ref a 0) (list-ref b 0)) #t)
       ((> (list-ref a 0) (list-ref b 0)) #f)
       (else #f)))))))


(define (date->string d)
  (string-append
   (number->string (list-ref d 0))
   "-"
   (substring (number->string (+ (list-ref d 1) 100)) 1 3)
   "-"
   (substring (number->string (+ (list-ref d 2) 100)) 1 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; random

(define random-maker
  (let* ((multiplier 48271)
         (modulus 2147483647)
         (apply-congruence
          (lambda (current-seed)
            (let ((candidate (modulo (* current-seed multiplier)
                                     modulus)))
              (if (zero? candidate)
                  modulus
                  candidate))))
         (coerce
          (lambda (proposed-seed)
            (if (integer? proposed-seed)
                (- modulus (modulo proposed-seed modulus))
                19860617))))  ;; an arbitrarily chosen birthday
  (lambda (initial-seed)
    (let ((seed (coerce initial-seed)))
      (lambda args
        (cond ((null? args)
               (set! seed (apply-congruence seed))
               (/ (- modulus seed) modulus))
              ((null? (cdr args))
               (let* ((proposed-top
                       (ceiling (abs (car args))))
                      (exact-top
                       (if (inexact? proposed-top)
                           (inexact->exact proposed-top)
                           proposed-top))
                      (top
                       (if (zero? exact-top)
                           1
                           exact-top)))
                 (set! seed (apply-congruence seed))
                 (inexact->exact (floor (* top (/ seed modulus))))))
              ((eq? (cadr args) 'reset)
               (set! seed (coerce (car args))))
              (else
               (display "random: unrecognized message")
               (newline))))))))

(define rand
  (random-maker 19781116))  ;; another arbitrarily chosen birthday

(define (random n)
  (abs (modulo (rand n) n)))

(define rndf rand)

(define (rndvec) (vector (rndf) (rndf) (rndf)))

(define (crndf)
  (* (- (rndf) 0.5) 2))

(define (crndvec)
  (vector (crndf) (crndf) (crndf)))

(define (srndvec)
  (let loop ((v (crndvec)))
    (if (> (vmag v) 1) ; todo: use non sqrt version
        (loop (crndvec))
        v)))

(define (hsrndvec)
  (let loop ((v (crndvec)))
    (let ((l (vmag v)))
      (if (or (> l 1) (eq? l 0))
          (loop (crndvec))
          (vdiv v l)))))

(define (grndf)
  (let loop ((x (crndf)) (y (crndf)))
    (let ((l (+ (* x x) (* y y))))
      (if (or (>= l 1) (eq? l 0))
          (loop (crndf) (crndf))
          (* (sqrt (/ (* -2 (log l)) l)) x)))))

(define (grndvec)
  (vector (grndf) (grndf) (grndf)))

(define (rndbary)
	(let*
		((a (- 1.0 (sqrt (rndf))))
		 (b (* (rndf) (- 1.0 a)))
		 (c (- 1.0 (+ a b))))
		(vector a b c)))

; return a line on the hemisphere
(define (rndhemi n)
  (let loop ((v (srndvec)))
    (if (> (vdot n v) 0)
        v
        (loop (srndvec)))))

(define (hrndhemi n)
  (let loop ((v (hsrndvec)))
    (if (> (vdot n v) 0)
        v
        (loop (hsrndvec)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (string-split str . rest)
		; maxsplit is a positive number
  (define (split-by-whitespace str maxsplit)
    (define (skip-ws i yet-to-split-count)
      (cond
        ((>= i (string-length str)) '())
        ((char-whitespace? (string-ref str i))
          (skip-ws (+ 1 i) yet-to-split-count))
        (else (scan-beg-word (+ 1 i) i yet-to-split-count))))
    (define (scan-beg-word i from yet-to-split-count)
      (cond
        ((zero? yet-to-split-count)
          (cons (substring str from (string-length str)) '()))
        (else (scan-word i from yet-to-split-count))))
    (define (scan-word i from yet-to-split-count)
      (cond
        ((>= i (string-length str))
          (cons (substring str from i) '()))
        ((char-whitespace? (string-ref str i))
          (cons (substring str from i)
            (skip-ws (+ 1 i) (- yet-to-split-count 1))))
        (else (scan-word (+ 1 i) from yet-to-split-count))))
    (skip-ws 0 (- maxsplit 1)))

		; maxsplit is a positive number
		; str is not empty
  (define (split-by-charset str delimeters maxsplit)
    (define (scan-beg-word from yet-to-split-count)
      (cond
        ((>= from (string-length str)) '(""))
        ((zero? yet-to-split-count)
          (cons (substring str from (string-length str)) '()))
        (else (scan-word from from yet-to-split-count))))
    (define (scan-word i from yet-to-split-count)
      (cond
        ((>= i (string-length str))
          (cons (substring str from i) '()))
        ((memv (string-ref str i) delimeters)
          (cons (substring str from i)
            (scan-beg-word (+ 1 i) (- yet-to-split-count 1))))
        (else (scan-word (+ 1 i) from yet-to-split-count))))
    (scan-beg-word 0 (- maxsplit 1)))

			; resolver of overloading...
			; if omitted, maxsplit defaults to
			; (inc (string-length str))
  (if (equal? str "") '()
    (if (null? rest)
      (split-by-whitespace str (+ 1 (string-length str)))
      (let ((charset (car rest))
          (maxsplit
            (if (pair? (cdr rest)) (cadr rest) (+ 1 (string-length str)))))
        (cond
          ((not (positive? maxsplit)) '())
          ((null? charset) (split-by-whitespace str maxsplit))
          (else (split-by-charset str charset maxsplit))))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; convert scheme values into equivilent json strings

(define (scheme->json v)
  (cond
   ((number? v) (number->string v))
   ((symbol? v) (string-append "\"" (symbol->string v) "\""))
   ((string? v) (string-append "\"" v "\""))
   ((boolean? v) (if v "true" "false"))
   ((list? v)
    (cond
     ((null? v) "[]")
     (else
      ; if it quacks like an assoc list...
      (if (and (not (null? v)) (not (list? (car v))) (pair? (car v)))
          (assoc->json v)
          (list->json v)))))
   (else "[]"))) ;;(display "value->js, unsupported type for ") (display v) (newline) "[]")))

(define (list->json l)
  (define (_ l s)
    (cond
     ((null? l) s)
     (else
      (_ (cdr l)
         (string-append
          s
          (if (not (string=? s "")) ", " "")
          (scheme->json (car l)))))))
  (string-append "[" (_ l "") "]"))

; ((one . 1) (two . "three")) -> { "one": 1, "two": "three }

(define (assoc->json l)
  (define (_ l s)
    (cond
     ((null? l) s)
     (else
      (let ((token (scheme->json (car (car l))))
            (value (scheme->json (cdr (car l)))))
        (_ (cdr l) (string-append s (if (not (string=? s "")) "," "")
                                  "\n" token ": " value))))))
  (string-append "{" (_ l "") "\n" "}"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; android ui

(define (layout width height weight gravity margin) (list "layout" width height weight gravity margin))
(define (rlayout width height margin rules) (list "relative-layout" width height margin rules))

(define (widget-type w) (list-ref w 0))
(define (widget-id w) (list-ref w 1))

;; all the widgets!
(define (linear-layout id orientation layout colour children)
  (list "linear-layout" id orientation layout colour children))
(define (linear-layout-children t) (list-ref t 5))
(define (relative-layout id layout colour children)
  (list "relative-layout" id layout colour children))
(define (relative-layout-children t) (list-ref t 4))
(define (frame-layout id layout children)
  (list "frame-layout" id layout children))
(define (frame-layout-children t) (list-ref t 3))
(define (scroll-view id layout children)
  (list "scroll-view" id layout children))
(define (scroll-view-vert id layout children)
  (list "scroll-view-vert" id layout children))
(define (scroll-view-children t) (list-ref t 3))
(define (draggable id orientation layout colour children listener)
  (list "draggable" id orientation layout colour children listener))
(define (draggable-children t) (list-ref t 5))
(define (draggable-listener t) (list-ref t 6))
(define (view-pager id layout fragment-list)
  (list "view-pager" id layout fragment-list))
(define (space layout) (list "space" "999" layout))
(define (space-view-layout t) (list-ref t 2))
(define (image-view id image layout) (list "image-view" id image layout))
(define (camera-preview id layout) (list "camera-preview" id layout))
(define (text-view id text size layout) (list "text-view" id text size layout))
(define (debug-text-view id text size layout) (list "debug-text-view" id text size layout))
(define (web-view id data layout) (list "web-view" id data layout))
(define (edit-text id text size type layout listener) (list "edit-text" id text size type layout listener))
(define (edit-text-listener t) (list-ref t 6))
(define (button id text text-size layout listener) (list "button" id text text-size layout listener))
(define (button-listener t) (list-ref t 5))
(define (image-button id image layout listener) (list "image-button" id image layout listener))
(define (image-button-listener t) (list-ref t 4))
(define (toggle-button id text text-size layout style listener) (list "toggle-button" id text text-size layout style listener))
(define (toggle-button-listener t) (list-ref t 6))
(define (seek-bar id max layout listener) (list "seek-bar" id max layout listener))
(define (seek-bar-listener t) (list-ref t 4))
(define (spinner id items layout listener) (list "spinner" id items layout listener))
(define (spinner-listener t) (list-ref t 4))
(define (canvas id layout drawlist) (list "canvas" id layout drawlist))
(define (canvas-drawlist t) (list-ref t 3))
(define (button-grid id type height textsize layout buttons listener)
  (list "button-grid" id type height textsize layout buttons listener))
(define (button-grid-listener b) (list-ref b 7))
(define (drawlist-line colour width points) (list "line" colour width points))
(define (drawlist-text text x y colour size align) (list "text" text x y colour size align))

(define (toast msg) (list "toast" 0 "toast" msg))
(define (play-sound wav) (list "play-sound" 0 "play-sound" wav))
(define (soundfile-start-recording wav) (list "soundfile-start-recording" 0 "soundfile-start-recording" wav))
(define (soundfile-stop-recording) (list "soundfile-stop-recording" 0 "soundfile-stop-recording"))
(define (soundfile-start-playback wav) (list "soundfile-start-playback" 0 "soundfile-start-playback" wav))
(define (soundfile-stop-playback) (list "soundfile-stop-playback" 0 "soundfile-stop-playback"))
(define (vibrate time) (list "vibrate" 0 "vibrate" time))
(define (make-directory name) (list "make-directory" 0 "make-directory" name))
;; treat this like a dialog so the callback fires
(define (list-files name path fn) (list "list-files" 0 "list-files" name fn path))
(define (gps-start name fn) (list "gps-start" 0 "gps-start" name fn))
(define (sensors-start name fn) (list "sensors-start" 0 "sensors-start" name fn))
(define (sensors-stop) (list "sensors-stop" 0 "sensors-stop"))
(define (sensors-get name fn) (list "sensors-get" 0 "sensors-get" name fn))
(define (delayed name delay fn) (list "delayed" 0 "delayed" name fn delay))
(define (network-connect name ssid fn) (list "network-connect" 0 "network-connect" name fn ssid))
(define (http-request name url fn) (list "http-request" 0 "http-request" name fn url))
(define (http-download name url filename) (list "http-download" 0 "http-download" name filename url))
(define (http-upload name url filename) (list "http-upload" 0 "http-upload" name filename url))
(define (send-mail to subject body attachments) (list "send-mail" 0 "send-mail" to subject body attachments))
(define (take-photo filename code) (list "take-photo" 0 "take-photo" filename code))
(define (process-image-in-place filename) (list "process-image-in-place" 0 "process-image-in-place" filename))
(define (walk-draggable name id fn) (list "walk-draggable" 0 "walk-draggable" name fn id))

(define (dialog-fragment id layout fragment-name fn)
  (list "dialog-fragment" 0 "dialog-fragment" id layout fragment-name fn))

(define (time-picker-dialog name fn)
  (list "time-picker-dialog" 0 "time-picker-dialog" name fn))
(define (date-picker-dialog name fn)
  (list "date-picker-dialog" 0 "date-picker-dialog" name fn))
(define (alert-dialog name msg fn)
  (list "alert-dialog" 0 "alert-dialog" name fn msg))
(define (dialog-type d) (list-ref d 2))
(define (dialog-name d) (list-ref d 3))
(define (dialog-fn d) (list-ref d 4))

(define (start-activity act request arg) (list "start-activity" 0 "start-activity" act request arg))
(define (start-activity-goto act request arg) (list "start-activity" 0 "start-activity-goto" act arg))
(define (finish-activity result) (list "finish-activity" 0 "finish-activity" result))

(define (build-fragment type id layout) (list "build-fragment" type id layout))
(define (replace-fragment id type) (list "replace-fragment" id type))

(define (update-widget type id token value) (list type id token value))
(define (update-widget-type l) (list-ref l 0))
(define (update-widget-id l) (list-ref l 1))
(define (update-widget-token l) (list-ref l 2))
(define (update-widget-value l) (list-ref l 3))

(define camera-properties '())
(define id-map ())
(define current-id 1)

;(define (find-id name id-map)
;  (cond
;   ((null? id-map) #f)
;   ((equal? name (car (car id-map))) (cadr (car id-map)))
;   (else (find-id name (cdr id-map)))))

;(define (get-id name)
;  (find-id name id-map))

;(define (make-id name)
;  (let ((existing (get-id name)))
;    (cond
;     (existing existing)
;     (else
;      (set! id-map (cons (list name current-id) id-map))
;      (set! current-id (+ current-id 1))
;      (get-id name)))))

;(define (get-id name)
;  (cadr (sorted-find id-map name)))

;(define (make-id name)
;  (prof-start "make-id")
;  (prof-start "make-id sorted find")
;  (let ((sf (sorted-find id-map name)))
;    (prof-end "make-id sorted find")
;    (let ((r (if (not sf)
;                 (let ((id current-id))
;                   (prof-start "make-id sorted add")
;                   (set! id-map (sorted-add id-map (list name id)))
 ;                  (prof-end "make-id sorted add")
;                   (set! current-id (+ current-id 1))
;                   id)
;                 (cadr sf))))
;      (prof-end "make-id")
;      r)))

(define (get-id name)
  (let ((r (id-map-get name)))
    (cond
     ((zero? r) (msg "no id found for" name) 0)
     (else
      ;;(msg "found id" name " " r)
      r))))

(define (make-id name)
  ;;(msg "making id for" name)
  (let ((id (id-map-get name)))
    (cond
     ((zero? id)
      ;;(msg "this is a new id")
     ; (prof-start "make-id")
      (id-map-add name current-id)
      (set! current-id (+ current-id 1))
     ; (prof-end "make-id")
      (- current-id 1))
     (else
      ;; seems scheme is shut down while the id store keeps going?
      (when (> id current-id) (set! current-id (+ id 1)))
      ;;(msg "we have seen this one before")
      ;;(msg id)
      id))))

(define prof-map '())

(define (new-prof-item id)
  (list id (time-now) 0 0))
(define (prof-item-id p) (list-ref p 0))
(define (prof-item-time p) (list-ref p 1))
(define (prof-item-accum p) (list-ref p 2))
(define (prof-item-calls p) (list-ref p 3))

(define (prof-item-restart p)
  (list
   (prof-item-id p)
   (time-now)
   (prof-item-accum p)
   (prof-item-calls p)))

(define (prof-item-end p)
  (list
   (prof-item-id p)
   0
   (+ (prof-item-accum p)
      (- (time-now) (prof-item-time p)))
   (+ (prof-item-calls p) 1)))

(define (prof-start id)
  (let ((dd (sorted-find prof-map id)))
    (if dd
        (set! prof-map
              (sorted-add
               prof-map (prof-item-restart dd)))
        (set! prof-map
              (sorted-add
               prof-map (new-prof-item id))))))

(define (prof-end id)
  (let ((d (sorted-find prof-map id)))
    (set! prof-map
          (sorted-add
           prof-map
           (prof-item-end d)))))

(define (prof-print)
  (let ((tot (foldl
              (lambda (d r)
                (+ (prof-item-accum d) r))
              0 prof-map)))
    (for-each
     (lambda (d)
       (msg (prof-item-id d)
            (prof-item-calls d)
             (prof-item-accum d)
             (* (/ (prof-item-accum d) tot) 100) "%"))
     prof-map)))

(define wrap (layout 'wrap-content 'wrap-content -1 'left 0))
(define fillwrap (layout 'fill-parent 'wrap-content -1 'left 0))
(define wrapfill (layout 'wrap-content 'fill-parent -1 'left 0))
(define fill (layout 'fill-parent 'fill-parent -1 'left 0))

(define (spacer size) (space (layout 'fill-parent size 1 'left 0)))


(define (horiz . l)
  (linear-layout
   0 'horizontal
   (layout 'fill-parent 'wrap-content -1 'centre 0)
   (list 0 0 0 0)
   l))

(define (horiz-colour col . l)
  (linear-layout
   0 'horizontal
   (layout 'fill-parent 'wrap-content -1 'centre 0)
   col
   l))

(define (vert . l)
  (linear-layout
   0 'vertical
   (layout 'fill-parent 'wrap-content 1 'centre 20)
   (list 0 0 0 0)
   l))

(define (vert-colour col . l)
  (linear-layout
   0 'vertical
   (layout 'fill-parent 'wrap-content 1 'centre 20)
   col
   l))

(define (vert-fill . l)
  (linear-layout
   0 'vertical
   (layout 'fill-parent 'fill-parent 1 'left 0)
   (list 0 0 0 0)
   l))

(define (relative rules colour . l)
  (relative-layout
   0 (rlayout 'fill-parent 'wrap-content (list 20 20 20 20) rules)
   colour
   l))

(define (activity name layout on-create on-start on-resume on-pause on-stop on-destroy on-activity-result)
  (list name layout on-create on-start on-resume on-pause on-stop on-destroy on-activity-result))

(define (fragment name layout on-create on-start on-resume on-pause on-stop on-destroy)
  (list name layout on-create on-start on-resume on-pause on-stop on-destroy))

(define (activity-name a) (list-ref a 0))
(define (activity-layout a) (list-ref a 1))
(define (activity-modify-layout a v) (list-replace a 1 v))
(define (activity-on-create a) (list-ref a 2))
(define (activity-on-start a) (list-ref a 3))
(define (activity-on-resume a) (list-ref a 4))
(define (activity-on-pause a) (list-ref a 5))
(define (activity-on-stop a) (list-ref a 6))
(define (activity-on-destroy a) (list-ref a 7))
(define (activity-on-activity-result a) (list-ref a 8))

(define (activity-list l) l)

(define (activity-list-find l name)
  (cond
   ((null? l) #f)
   ((equal? (activity-name (car l)) name) (car l))
   (else (activity-list-find (cdr l) name))))

(define activities 0)
(define fragments 0)

(define callbacks '())
(define (callback id type fn) (list id type fn))
(define (callback-id l) (list-ref l 0))
(define (callback-type l) (list-ref l 1))
(define (callback-fn l) (list-ref l 2))
(define (find-callback id) (sorted-findv callbacks id))
(define (add-callback! cb)
  ;;(msg "adding" cb)
  (set! callbacks (sorted-addv callbacks cb)))

(define (widget-get-children w)
  (cond
   ((equal? (widget-type w) "linear-layout") (linear-layout-children w))
   ((equal? (widget-type w) "relative-layout") (relative-layout-children w))
   ((equal? (widget-type w) "frame-layout") (frame-layout-children w))
   ((equal? (widget-type w) "scroll-view") (scroll-view-children w))
   ((equal? (widget-type w) "scroll-view-vert") (scroll-view-children w))
   ((equal? (widget-type w) "draggable") (draggable-children w))
;;   ((equal? (widget-type w) "grid-layout") (grid-layout-children w))
   (else '())))

(define (widget-get-callback w)
  (cond
   ((equal? (widget-type w) "edit-text") (edit-text-listener w))
   ((equal? (widget-type w) "button") (button-listener w))
   ((equal? (widget-type w) "image-button") (image-button-listener w))
   ((equal? (widget-type w) "toggle-button") (toggle-button-listener w))
   ((equal? (widget-type w) "seek-bar") (seek-bar-listener w))
   ((equal? (widget-type w) "spinner") (spinner-listener w))
   ((equal? (widget-type w) "button-grid") (button-grid-listener w))
   ((equal? (widget-type w) "draggable") (draggable-listener w))
   (else #f)))

;; walk through activity stripping callbacks
;; version called from on-create
(define (update-callbacks! widget-list)
  (cond
   ((null? widget-list) #f)
   (else
    (let* ((w (car widget-list))
           (c (widget-get-children w)))
      (when (not (null? c))
            (update-callbacks! c))
      (let ((cb (widget-get-callback w)))
        (when cb
              (add-callback! (callback (widget-id w) (widget-type w) cb)))))
    (update-callbacks! (cdr widget-list)))))

;; walk through update stripping callbacks
;; version called with update-widgets (after on-create version above)
(define (update-callbacks-from-update! widget-list)
  (if (null? widget-list) #f
      (let ((w (car widget-list)))
        (cond
         ((null? w) #f)
         ;; drill deeper
         ((eq? (update-widget-token w) 'contents)
          (update-callbacks! (update-widget-value w)))
         ((eq? (update-widget-token w) 'contents-add)
          (update-callbacks! (update-widget-value w)))
         ((eq? (update-widget-token w) 'grid-buttons)
          (add-callback! (callback (update-widget-id w)
                                   "button-grid"
                                   (list-ref (update-widget-value w) 5)))))
        (update-callbacks-from-update! (cdr widget-list)))))

(define (define-activity-list . args)
  (set! activities (activity-list args)))

(define (define-fragment-list . args)
  (set! fragments (activity-list args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; replace with new cb system

(define dialogs '())

(define (dialog-find dl name)
  (cond
   ((null? dl) #f)
   ((equal? (dialog-name (car dl)) name) (car dl))
   (else (dialog-find (cdr dl) name))))

(define (dialog-replace dl name d)
  (cond
   ((null? dl) (list d))
   ((equal? (dialog-name (car dl)) name)
    (cons d (cdr dl)))
   (else (cons (car dl) (dialog-replace (cdr dl) name d)))))


(define (add-new-dialog! d)
  (set! dialogs (dialog-replace dialogs (dialog-name d) d))
  ;; todo - when to clear out?
  ;;(when (not (dialog-find dialogs (dialog-name d)))
        ;;(display "adding dialog ")(display d)(newline)
  ;;      (set! dialogs (cons d dialogs)))
  )


(define (update-dialogs! events)
  (when (list? events)
        (for-each
         (lambda (event)
           (when (or
                  ;; todo - something a bit more fancy
                  (equal? (list-ref event 0) "date-picker-dialog")
                  (equal? (list-ref event 0) "alert-dialog")
                  (equal? (list-ref event 0) "list-files")
                  (equal? (list-ref event 0) "http-request")
                  (equal? (list-ref event 0) "network-connect")
                  (equal? (list-ref event 0) "delayed")
                  (equal? (list-ref event 0) "walk-draggable")
                  (equal? (list-ref event 0) "sensors-start")
                  (equal? (list-ref event 0) "sensors-get")
                  (equal? (list-ref event 0) "gps-start"))
                 (add-new-dialog! event)))
         events)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (dialog-callback name args)
  (let ((dialog (dialog-find dialogs name)))
    (if (not dialog)
        (begin (display "no dialog called ")(display name)(newline))
        (let ((events (apply (dialog-fn dialog) args)))
          (update-dialogs! events)
          (update-callbacks-from-update! events)
          (send (scheme->json events))))))

;; called by java
(define (activity-callback type activity-name args)
  (prof-start "activity-callback")
  (let ((r (let ((activity (activity-list-find activities activity-name)))
             (top-callback type activity-name activity args))))
    (prof-end "activity-callback")
    r))

;; called by java
(define (fragment-callback type fragment-name args)
  (prof-start "activity-callback")
  (let ((r (let ((fragment (activity-list-find fragments fragment-name)))
             (top-callback type fragment-name fragment args))))
    (prof-end "activity-callback")
    r))

(define (top-callback type activity-name activity args)
  ;;(display "activity/fragment-callback ")(display type)(display " ")(display args)(newline)
  (if (not activity)
      (begin (display "no activity/fragment called ")(display activity-name)(newline))
      (let ((ret (cond
                  ;; todo update activity...?
                  ((eq? type 'on-create) ((activity-on-create activity) activity (car args)))
                  ((eq? type 'on-start) ((activity-on-start activity) activity (car args)))
                  ((eq? type 'on-stop) ((activity-on-stop activity) activity))
                  ((eq? type 'on-resume) ((activity-on-resume activity) activity))
                  ((eq? type 'on-pause) ((activity-on-pause activity) activity))
                  ((eq? type 'on-destroy) ((activity-on-destroy activity) activity))
                  ((eq? type 'on-activity-result) ((activity-on-activity-result activity) activity (car args) (cadr args)))
                  (else
                   (display "no callback called ")(display type)(newline)
                   '()))))
        (cond
         ((eq? type 'on-create)
          (update-callbacks! (list ret)))
         (else
          (update-dialogs! ret)
          (update-callbacks-from-update! ret)))
        (send (scheme->json ret)))))

(define (find-activity-or-fragment name)
  (let ((r (activity-list-find activities name)))
    (if r r
        (activity-list-find fragments name))))

(define (widget-callback activity-name widget-id args)
  (prof-start "widget-callback")
  (let ((cb (find-callback widget-id)))
    (if (not cb)
        (msg "no widget" widget-id "found!")
        (let ((events
               (cond
                ((equal? (callback-type cb) "edit-text")
                 ((callback-fn cb) (car args)))
                ((equal? (callback-type cb) "button")
                 ((callback-fn cb)))
                ((equal? (callback-type cb) "image-button")
                 ((callback-fn cb)))
                ((equal? (callback-type cb) "toggle-button")
                 ((callback-fn cb) (car args)))
                ((equal? (callback-type cb) "seek-bar")
                 ((callback-fn cb) (car args)))
                ((equal? (callback-type cb) "spinner")
                 ((callback-fn cb) (car args)))
                ((equal? (callback-type cb) "button-grid")
                 ((callback-fn cb) (car args) (cadr args)))
                ((equal? (callback-type cb) "draggable")
                 ((callback-fn cb)))
                (else
                 (msg "no callbacks for type" (callback-type cb))))))
          ;; this was just update-callbacks, commented out,
          ;; expecting trouble here... (but seems to fix new widgets from
          ;; widget callbacks so far)
          (update-callbacks-from-update! events)
          (update-dialogs! events)
          (send (scheme->json events))
          (prof-end "widget-callback")))))


(alog "lib.scm done")
