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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; strings


;; colours
(msg "starting up....")
(define entity-types (list "user-data"))

(define trans-col (list 0 0 0 0))
(define colour-one (list 0 0 255 100))
(define colour-two (list  127 127 255 100))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; persistent database

(define db "/sdcard/uav-toolkit/uav-toolkit.db")
(db-open db)
(setup db "local")
(setup db "code")

(define settings-entity-id-version 2)

(insert-entity-if-not-exists
 db "local" "app-settings" "null" settings-entity-id-version
 (list
  (ktv "user-id" "varchar" "not set")
  (ktv "language" "int" 0)
  (ktv "current-village" "varchar" "none")))

(define (get-setting-value name)
  (ktv-get (get-entity db "local" settings-entity-id-version) name))

(define (set-setting! key type value)
  (update-entity
   db "local" settings-entity-id-version (list (ktv key type value))))

(define (get/inc-setting key)
  (let ((r (get-setting-value key)))
    (set-setting! key "int" (+ r 1))
    r))

(define code-version 5)

(insert-entity-if-not-exists
 db "code" "program" "null" code-version
 (list
  (ktv "text" "varchar" (scheme->json '(when-timer 3 (toast (number->string (+ 2 3 4))))))))

(set-current! 'user-id (get-setting-value "user-id"))
(set! i18n-lang (get-setting-value "language"))

;;(display (db-all db "local" "app-settings"))(newline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 (define-macro (when-timer . args)
  `(begin
     (define (when-timer-cb)
       ;;(msg ,(cdr args))
       (list
        ,(cadr args)
        (delayed "when-timer" (* 1000 ,(car args)) when-timer-cb)))
     (list
      (delayed "when-timer" (* 1000 ,(car args)) when-timer-cb))))

(define (sensor->ktv-list d)
  (let ((name (car d)))
    (index-map
     (lambda (i d)
       (if (number? d)
           (ktv (string-append name "-" (number->string i)) "real" d)
           (ktv (string-append name "-" (number->string i)) "varchar" d)))
     (cadr d))))

(define (save-entity name . data)
  (entity-create!
   db "stream" "user-data"
   (cons
    (ktv "name" "varchar" name)
    (foldl
     (lambda (i r)
       (if i (append r (sensor->ktv-list i)) r))
     '()
     (dbg data))))
  (toast (string-append "saved entity: " name)))

(define (show . data)
  (if data
      (toast-size
       (foldl
        (lambda (d r)
          (if d
              (string-append r " " (escape-quotes (scheme->json d)))
              (string-append r " no data yet...")))
        "" data)
       10)
      (toast "no data yet...")))

(define (get-sensor-value type)
  (set-current! 'sensors (set-add type (get-current 'sensors '())))
  (let ((item (assv type (get-current 'sensor-values '()))))
    (if item (list (sensor-type->string (car item)) (cadr item)) #f)))

(define (accelerometer) (get-sensor-value sensor-accelerometer))
(define (ambient-temperature) (get-sensor-value sensor-ambient-temperature))
(define (game-rotation-vector) (get-sensor-value sensor-game-rotation-vector))
(define (gravity) (get-sensor-value sensor-gravity))
(define (gyroscope) (get-sensor-value sensor-gyroscope))
(define (gyroscope-uncalibrated) (get-sensor-value sensor-gyroscope-uncalibrated))
(define (light) (get-sensor-value sensor-light))
(define (linear-acceleration) (get-sensor-value sensor-linear-acceleration))
(define (magnetic-field) (get-sensor-value sensor-magnetic-field))
(define (magnetic-field-uncalibrated) (get-sensor-value sensor-magnetic-field-uncalibrated))
(define (orientation) (get-sensor-value sensor-orientation))
(define (pressure) (get-sensor-value sensor-pressure))
(define (proximity) (get-sensor-value sensor-pressure))
(define (relative-humidity) (get-sensor-value sensor-relative-humidity))
(define (rotation-vector) (get-sensor-value sensor-rotation-vector))
(define (significant-motion) (get-sensor-value sensor-significant-motion))

(define (sensor-type->string s)
  (cond
   ((equal? s sensor-accelerometer) "accelerometer")
   ((equal? s sensor-ambient-temperature) "ambient-temperature")
   ((equal? s sensor-game-rotation-vector) "game-rotation-vector")
   ((equal? s sensor-gravity) "gravity")
   ((equal? s sensor-gyroscope) "gyroscope")
   ((equal? s sensor-gyroscope-uncalibrated) "gyroscope-uncalibrated")
   ((equal? s sensor-light) "light")
   ((equal? s sensor-linear-acceleration) "linear-acceleration")
   ((equal? s sensor-magnetic-field) "magnetic-field")
   ((equal? s sensor-magnetic-field-uncalibrated) "magnetic-field-uncalibrated")
   ((equal? s sensor-orientation) "orientation")
   ((equal? s sensor-pressure) "pressure")
   ((equal? s sensor-proximity) "proximity")
   (else "unknown-sensor")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define did 100)
(define (new-id)
  (set! did (+ did 1))
  (number->string did))

(define (inner-code-blockify code)
  (msg "inner:" code)
  (cond
   ((null? code) (code-block "list" '()))
   ((number? code) (number-code-block code))
   ((symbol? code) (code-block (symbol->string code) '()))
   ((string? code)
    (if (code-block-known? code)
        (code-block code '())
        (text-code-block code)))
   ((list? code)
    (if (string? (car code));; ignore first 'list'
        (code-block (car code)
                    (map inner-code-blockify (cdr code)))
        (code-block "list" (map inner-code-blockify code))))
   (else (code-block "error" '()))))

(define (text->code-block text)
  (msg "text->codeblock" text)
  (let ((code (json/parse-string text)))
    (map inner-code-blockify code)))

(define (load-code)
  (msg "loading code")
  (text->code-block (dbg (ktv-get (get-entity db "code" code-version) "text"))))

(define (code-block text children)
  (cond 
   ;; dispatch to special forms
   ((equal? text "text") (text-code-block text '()))
   ((equal? text "camera") (camera-code-block text))
   (else
    (let ((id (new-id)))
      (draggable
       (make-id (string-append id "-code-block"))
       'vertical wrap (code-block-colour text)
       (if (code-block-atom? text) "drag-only" "normal")
       (append
        (list
         (text-view 0 text 30 wrap))
        children)
       (lambda ()
         (scheme->json
          (list
           (if (code-block-atom? text) 1 0) text))))))))

(define (number-code-block num)
  (let ((id (new-id)))
    (draggable
     (make-id (string-append id "-code-block"))
     'vertical wrap (list 255 255 255 255)
     "drag-only"
     (list
      (edit-text (make-id (string-append id "-edit"))
                 (number->string num) 30 "numeric" wrap
                 (lambda (v)
                   ;; clearly this is dubious, but it works...!
                   (set! num (string->number v))
                   '())))
     (lambda ()
       (scheme->json (list 1 num))))))

(define (text-code-block text)
  (let ((id (new-id)))
    (draggable
     (make-id (string-append id "-code-block"))
     'vertical wrap (list 255 255 255 255)
     "drag-only"
     (list
      (edit-text (make-id (string-append id "-edit"))
                 text 30 "normal" wrap
                 (lambda (v)
                   ;; clearly this is dubious, but it works...!
                   (set! text v)
                   '())))
     (lambda ()
       (scheme->json (list 1 (string-append "\\\"" text "\\\"")))))))


(define (camera-code-block text)
  (let ((id (new-id)))
    (draggable
     (make-id (string-append id "-code-block"))
     'vertical wrap (list 255 255 255 255)
     "drag-only"
     (list
      (camera-preview (make-id "camerap") (layout 'fill-parent 320 1 'left 0)))

     (lambda ()
       (list 1 text)))))



(define control-colour (list 255 200 100 255))
(define control-functions
  (list "when-timer" "when-moved-metres" "when-update" "when-pressed"))

(define data-colour (list 200 255 100 255))
(define data-functions
  (list "save-entity" "load-entity" "text"))

(define sensor-colour (list 255 100 200 255))
(define sensor-functions
  (list
  "accelerometer"
  "ambient-temperature"
  "game-rotation-vector"
  "gravity"
  "gyroscope"
  "gyroscope-uncalibrated"
  "light"
  "linear-acceleration"
  "magnetic-field"
  "magnetic-field-uncalibrated"
  "orientation"
  "pressure"
  "proximity"
  "relative-humidity"
  "rotation-vector"
  "significant-motion"

  "gps"
  "camera"
  "camera-horiz-angle"
  "camera-vert-angle"))

(define maths-colour (list 200 100 255 255))
(define maths-functions
  (list "+" "-" "/" "*" "sin" "cos" "tan" "asin" "acos" "atan" "modulo" "pow" "300" "\\\"hello world\\\""))

(define display-colour (list 100 255 200 255))
(define display-functions
  (list "show" "sound" "vibrate"))

(define (code-block-colour text)
  (cond
   ((string-in-list-fast text control-functions) control-colour)
   ((string-in-list-fast text data-functions) data-colour)
   ((string-in-list-fast text sensor-functions) sensor-colour)
   ((string-in-list-fast text maths-functions) maths-colour)
   ((string-in-list-fast text display-functions) display-colour)
   (else (list 255 0 255 255))))

(define (code-block-known? text)
  (or
   (string-in-list-fast text control-functions)
   (string-in-list-fast text data-functions)
   (string-in-list-fast text sensor-functions)
   (string-in-list-fast text maths-functions)
   (string-in-list-fast text display-functions)))

(define (code-block-atom? text)
  (string-in-list-fast text (append '("300"))))


(define (build-menu fns)
  (append
   (list
    (update-widget
     'linear-layout (get-id "block-menu")
     'contents
     (map
      (lambda (fn)
        (button (make-id (string-append fn "-button"))
                fn 30 (layout 'fill-parent 'wrap-content 1 'left 5)
                (lambda ()
                  (list
                   (update-widget
                    'draggable (get-id "block-root")
                    'contents (list (code-block fn '())))))))
      fns)))
   (map
    (lambda (fn)
      (update-widget 'button (get-id (string-append fn "-button"))
                     'background-colour (code-block-colour (car fns))))
    fns)
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; review data stuff

(define (medit-text-value id text value type fn)
  (linear-layout
   0 'horizontal
   (layout 'fill-parent 'wrap-content -1 'centre 2)
   (list 0 0 0 10)
  (list
   (text-view (make-id (string-append id "-title")) text 20 (layout 'fill-parent 'wrap-content 1 'right 0))
   (edit-text (make-id id) value 20 type (layout 'fill-parent 'wrap-content 1 'centre 0) fn))))

(define (review-build-contents uid entity)
  (append
   (foldl
    (lambda (ktv r)
      (if (or (equal? (ktv-key ktv) "unique_id")
              (equal? (ktv-key ktv) "user")
              (equal? (ktv-key ktv) "deleted"))
          r
          (append
           r (cond
              ((equal? (ktv-type ktv) "varchar")
               ;; normal varchar
               (list (medit-text-value (string-append uid (ktv-key ktv))
                                       (string-append (ktv-key ktv) " : " (ktv-type ktv))
                                       (ktv-value ktv) "normal"
                                       (lambda (v)
                                         (entity-set-value-mem! (ktv-key ktv) (ktv-type ktv) v) '()))))
              ((equal? (ktv-type ktv) "file")
               ;; normal varchar
               (list (medit-text-value (string-append uid (ktv-key ktv))
                                       (string-append (ktv-key ktv) " : " (ktv-type ktv))
                                       (ktv-value ktv) "normal"
                                       (lambda (v)
                                         (entity-set-value-mem! (ktv-key ktv) (ktv-type ktv) v) '()))))
              ((equal? (ktv-type ktv) "int")
               (list (medit-text-value (string-append uid (ktv-key ktv))
                                       (string-append (ktv-key ktv) " : " (ktv-type ktv))
                                       (number->string (ktv-value ktv)) "numeric"
                                       (lambda (v)
                                         (entity-set-value-mem! (ktv-key ktv) (ktv-type ktv) v) '()))))
              ((equal? (ktv-type ktv) "real")
               (list (medit-text-value (string-append uid (ktv-key ktv))
                                       (string-append (ktv-key ktv) " : " (ktv-type ktv))
                                       ;; get around previous bug, should remove
                                       (if (number? (ktv-value ktv))
                                           (number->string (ktv-value ktv))
                                           (ktv-value ktv)) "numeric"
                                           (lambda (v)
                                             (entity-set-value-mem! (ktv-key ktv) (ktv-type ktv) v) '()))))
              (else (mtext 0 (string-append (ktv-type ktv) " not handled!!")) '())))))
    '()
    entity)
   (list
    (horiz
     (button (make-id "review-item-cancel") "Cancel" 20 (layout 'fill-parent 'wrap-content 1 'centre 0) (lambda () (list (finish-activity 0))))
     (button (make-id (string-append uid "-save")) "Save" 20 (layout 'fill-parent 'wrap-content 1 'centre 0)
              (lambda ()
                (list
                 (alert-dialog
                  "review-ok"
                  (string-append "Are you sure?")
                  (lambda (v)
                    (cond
                     ((eqv? v 1)
                      (entity-update-values!)
                      (list))
                     (else (list))))))))))))


(define (review-item-build)
  (let ((uid (entity-get-value "unique_id")))
    (list
     (update-widget
      'linear-layout
      (get-id "review-item-container")
      'contents
      (review-build-contents
       uid (get-current 'entity-values '()))))))

(define (review-update-list entity-type)
  (list
   (update-widget
    'linear-layout (get-id "review-list") 'contents
    (map
     (lambda (e)
       (let* ((uid (ktv-get e "unique_id")))
         (msg e)
         (button
          (make-id (string-append "review-" uid))
          (string-append (ktv-get e "name") " " (ktv-get e "time"))
          20 fillwrap
          (lambda ()
            (entity-init! db "stream" entity-type (get-entity-by-unique db "stream" uid))
            (list (start-activity "review-item" 0 ""))))))
     (db-filter-only-inc-deleted db "stream" entity-type
                                 (list)
                                 (list (list "name" "varchar")
                                       (list "time" "varchar")))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fragments

(define-fragment-list
  (fragment
   "block-chooser"
   (linear-layout
    0 'vertical fill (list 0 0 0 0)
    (list
     (scroll-view
      0 fillwrap
      (list
       (horiz
        (mbutton-scale 'control (lambda () (build-menu control-functions)))
        (mbutton-scale 'data (lambda () (build-menu data-functions)))
        (mbutton-scale 'sensors (lambda () (build-menu sensor-functions)))
        (mbutton-scale 'maths (lambda () (build-menu maths-functions)))
        (mbutton-scale 'display (lambda () (build-menu display-functions)))
        )))
     (scroll-view-vert
      0 fillwrap
      (list
       (linear-layout
        (make-id "block-menu") 'vertical fill (list 0 0 0 0)
        (list))))
     ))
   (lambda (fragment arg)
     (activity-layout fragment))
   (lambda (fragment arg)
     (append
      (build-menu control-functions)
      (list
       (update-widget 'button (get-id "control") 'background-colour control-colour)
       (update-widget 'button (get-id "data") 'background-colour data-colour)
       (update-widget 'button (get-id "sensors") 'background-colour sensor-colour)
       (update-widget 'button (get-id "maths") 'background-colour maths-colour)
       (update-widget 'button (get-id "display") 'background-colour display-colour)
       )))
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '()))



  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; activities

(define photo-code 999)

(define-activity-list

  (activity
   "main"
   (vert
    (image-view 0 "logo" (layout 'wrap-content 'wrap-content -1 'centre 0))

    (scroll-view-vert
     0 (layout 'fill-parent 'wrap-content 0.75 'centre 0)
     (list
      (linear-layout
       (make-id "button-list") 'vertical
       (layout 'fill-parent 'wrap-content 0.75 'centre 0)
       (list 0 0 0 0)
       (list
     (button
      (make-id "main-sensors")
      "Peek at your sensors"
      30 (layout 'fill-parent 'wrap-content -1 'centre 5)
      (lambda ()
        (list (start-activity "sensor" 0 ""))))
     (button
      (make-id "main-camera")
      "Check the camera"
      30 (layout 'fill-parent 'wrap-content -1 'centre 5)
      (lambda ()
        (list (start-activity "camera" 0 ""))))
     (button
      (make-id "vis-prog")
      "Visual programming"
      30 (layout 'fill-parent 'wrap-content -1 'centre 5)
      (lambda ()
        (list (start-activity "vptest" 0 ""))))
     (button
      (make-id "review")
      "View data"
      30 (layout 'fill-parent 'wrap-content -1 'centre 5)
      (lambda ()
        (list (start-activity "review" 0 ""))))

     ))))
    )

   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (list))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))

  (activity
   "sensor"
   (vert
    (image-view 0 "logo" (layout 'wrap-content 'wrap-content -1 'centre 0))

    (scroll-view-vert
     0 (layout 'fill-parent 'wrap-content 0.75 'centre 0)
     (list
      (linear-layout
       (make-id "sensor-list") 'vertical
       (layout 'fill-parent 'wrap-content 0.75 'centre 0)
       (list 0 0 0 0)
       (list))))

    (horiz
     (button
      (make-id "sensors-start")
      "Start"
      40 (layout 'wrap-content 'wrap-content 1 'centre 5)
      (lambda ()
        (list
         (sensors-start
          "start-sensors"
          (dbg (list
           sensor-accelerometer
           sensor-ambient-temperature
           sensor-game-rotation-vector
           sensor-gravity
           sensor-gyroscope
           sensor-gyroscope-uncalibrated
           sensor-light
           sensor-linear-acceleration
           sensor-magnetic-field
           sensor-magnetic-field-uncalibrated
           sensor-orientation
           sensor-pressure
           sensor-proximity
           sensor-relative-humidity
           sensor-rotation-vector
           sensor-significant-motion))
          (lambda (data)
            (list
             (update-widget
              'text-view
              (get-id (string-append (list-ref data 0) "-values")) 'text
              (apply
               string-append
               (map (lambda (d) (string-append (number->string d) " "))
                    (cdr (cdr (cdr (cdr data)))))))))))))
     (button
      (make-id "sensors-stop")
      "Stop"
      40 (layout 'wrap-content 'wrap-content 1 'centre 5)
      (lambda () (list (sensors-stop)))))

    (button
     (make-id "sensors-back")
     "Back"
     40 (layout 'fill-parent 'wrap-content 1 'centre 5)
     (lambda ()
       (list (finish-activity 0)))))

   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (list
      (sensors-get
       "build-sensors-cb"
       (lambda (data)
         (list
          (update-widget 'linear-layout (get-id "sensor-list") 'contents
                         (map
                          (lambda (sensor)
                            (linear-layout
                             0 'vertical
                             (layout 'fill-parent 'wrap-content 0.75 'centre 5)
                             (list 255 255 0 255)
                             (list
                              (text-view (make-id (list-ref sensor 0)) (list-ref sensor 0) 20 fillwrap)
                              (text-view (make-id (dbg (string-append (list-ref sensor 0) "-values")))
                                         "Nothing yet..." 15 fillwrap))))
                          data)))))
      ))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))

  (activity
   "camera"
   (horiz
    (vert
     (camera-preview (make-id "camerap") (layout 'fill-parent 320 1 'left 0))

     (horiz
     (button
      (make-id "get-camera-props")
      "Properties"
      20 (layout 'wrap-content 'wrap-content 1 'centre 5)
      (lambda ()
        (list
         (update-widget 'linear-layout (get-id "camera-props") 'contents
                       (map
                        (lambda (p)
                          (linear-layout
                           0 'horizontal
                           (layout 'fill-parent 'wrap-content 0.75 'centre 5)
                           (list 255 255 0 255)
                           (list
                            (text-view 0 (list-ref p 0) 20 (layout 'fill-parent 'wrap-content 1 'left 0))
                            (text-view 0 (list-ref p 1) 20 (layout 'fill-parent 'wrap-content 1 'left 0)))))
                        camera-properties)
                       ))))


     (button
      (make-id "camera-back")
      "Back"
      20 (layout 'wrap-content 'wrap-content 1 'centre 5)
      (lambda ()
        (list
         (update-widget 'camera-preview (get-id "camerap") 'shutdown 0)
         (finish-activity 0))))
     ))

    (scroll-view-vert
     0 (layout 'fill-parent 'wrap-content 0.75 'centre 0)
     (list
      (linear-layout
       (make-id "camera-props") 'vertical
       (layout 'fill-parent 'wrap-content 0.75 'centre 0)
       (list 0 0 0 0)
       (list))))


    )

   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (list))
   (lambda (activity)
     (list (update-widget 'camera-preview (get-id "camerap") 'shutdown 0)))
   (lambda (activity) '())
   (lambda (activity)
     (list (update-widget 'camera-preview (get-id "camerap") 'shutdown 0)))
   (lambda (activity)
     (list (update-widget 'camera-preview (get-id "camerap") 'shutdown 0)))
   (lambda (activity requestcode resultcode) '()))

  (activity
   "vptest"

  (vert-fill
   (relative
    '(("parent-top"))
    (list 0 0 0 0)
    (vert
     (horiz
      (mtoggle-button-scale
       'add
       (lambda (v)
         (if (eqv? v 1)
             (list (replace-fragment (make-id "menu-holder") "block-chooser"))
             (list (replace-fragment (make-id "menu-holder") "")))))
      
      (mbutton-scale
       'eval
       (lambda ()
         (list
          (walk-draggable
           "eval-walk" (get-id "block-root")
           (lambda (t)
             (msg "evaling->" t)
             (append
              (dbg (foldl (lambda (sexp r)
                            (append (eval sexp) r)) '() t) )
              (list
               (sensors-start
                "start-sensors"
                (dbg (get-current 'sensors '()))
                (lambda (data)
                  (set-current! 'sensor-values
                                (addv
                                 (get-current 'sensor-values '())
                                 (list (list-ref data 1)
                                       (cdr (cdr (cdr (cdr data)))))))
                  '())))))))))
      (mbutton-scale
       'save
       (lambda ()
         (list
          (walk-draggable
           "eval-walk" (get-id "block-root")
           (lambda (t)
             (msg "save code")
             (update-entity
              db "code" code-version
              (list
               (ktv "text" "varchar" (dbg (scheme->json t)))))
             (list
              (toast (string-append "saved"))))))))
      )
     
     (build-fragment
      "" (make-id "menu-holder")
      (layout 'fill-parent 'wrap-content -1 'left 0))))




   (scroll-view-vert
    0 (layout 'fill-parent 'fill-parent 1 'centre 0)
    (list
     (vert-fill
      (mtext 'code)

      (scroll-view-vert
       0 (layout 'fill-parent 'fill-parent 1 'centre 0)
       (list
        (draggable
         (make-id "block-root")
         'vertical (layout 'fill-parent 'fill-parent 1 'left 0) (list 255 255 0 20)
         "drop-only"
         (list)
         (lambda ()
           (msg "root cb")
           (scheme->json (list 0 "")))))))))




   (relative
    '(("parent-bottom"))
    (list 0 0 0 0)
     (vert
      
      (draggable
       (make-id "block-bin")
       'vertical (layout 'fill-parent 'fill-parent -1 'left 0) (list 255 255 0 20)
       "drop-only-consume"
       (list (mtext 'rubbish-bin))
       (lambda ()
         (msg "rubbish bin cb")
         (scheme->json (list 0 "")))))) 
    )
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (list
      (update-widget
       'draggable (get-id "block-root")
       'contents (load-code))))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode)
     (list)))

  (activity
   "review"
   (vert
    (horiz
     (text-view 0 "View data" 40 (layout 'fill-parent 'wrap-content 1 'left 0))
     (spinner (make-id "entity-type-spinner")
              entity-types
              (layout 'fill-parent 'wrap-content 1 'centre 0)
              (lambda (v)
                (review-update-list
                 (list-ref entity-types v)))))


    (scroll-view-vert
     0 (layout 'fill-parent 'wrap-content 1 'left 0)
     (list
      (linear-layout
       (make-id "review-list")
       'vertical
       (layout 'fill-parent 'fill-parent 1 'left 0)
       (list 0 0 0 0)
       (list))
      )))
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))

  (activity
   "review-item"
   (vert
    (text-view (make-id "title") "Edit item" 40 fillwrap)
    (scroll-view-vert
     0 (layout 'fill-parent 'wrap-content 1 'left 0)
     (list
      (linear-layout
       (make-id "review-item-container")
       'vertical
       (layout 'fill-parent 'wrap-content 1 'left 0)
       (list 0 0 0 0)
       (list))))
    )
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (review-item-build))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))



  )


;(build-test! db "sync" village-ktvlist household-ktvlist individual-ktvlist)
