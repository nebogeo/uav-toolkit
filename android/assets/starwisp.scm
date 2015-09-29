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
(define entity-types (list "user-data"))

(define trans-col (list 0 0 0 0))
(define colour-one (list 0 0 255 100))
(define colour-two (list  127 127 255 100))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; persistent database

(define db "/sdcard/uavtoolkit/uav-toolkit.db")
(db-open db)
(setup db "local")
(setup db "code")
(setup db "stream")

(define settings-entity-id-version 2)

(define (insert-if-not-exists-name db table type name ktv-list)
  (when
   (null? (filter-entities-inc-deleted db table type (list (list "name" "varchar" "=" name))))
   (insert-entity db table type "sys" ktv-list)))

(insert-entity-if-not-exists
 db "local" "app-settings" "null" settings-entity-id-version
 (list
  (ktv "user-id" "varchar" "not set")
  (ktv "language" "int" 0)
  (ktv "current-village" "varchar" "none")))

(insert-if-not-exists-name
 db "code" "program" "camera fov 2"
 (list
  (ktv "name" "varchar" "camera fov 2")
  (ktv "text" "varchar" (scheme->json
                         '((when-moved-metres
                            (* (tan (to-radians (/ (camera-vert-angle) 2)))
                               (* 30 0.5))
                            (save-to-db "camera-fov"
                                        (orientation)
                                        (gyroscope)
                                        (gravity)
                                        (accelerometer)
                                        (magnetic-field)
                                        (gps)
                                        (take-photo))
                            (noise)))))))

(insert-if-not-exists-name
 db "code" "program" "timed camera 2"
 (list
  (ktv "name" "varchar" "timed camera 2")
  (ktv "text" "varchar" (scheme->json
                         '((when-timer
                            3
                            (save-to-db
                             "camera-timer"
                             (orientation)
                             (gyroscope)
                             (gravity)
                             (accelerometer)
                             (magnetic-field)
                             (gps)
                             (take-photo))
                            (noise)))))))

(insert-if-not-exists-name
 db "code" "program" "new location camera"
 (list
  (ktv "name" "varchar" "new location camera")
  (ktv "text" "varchar" (scheme->json
                         '((when-in-new-location
                            (* (tan (to-radians (/ (camera-vert-angle) 2)))
                               (* 30 0.5))
                            (save-to-db "camera-new-location"
                                        (orientation)
                                        (gyroscope)
                                        (gravity)
                                        (accelerometer)
                                        (magnetic-field)
                                        (gps)
                                        (take-photo))
                            (noise)))))))



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

(set-current! 'user-id (get-setting-value "user-id"))
(set! i18n-lang (get-setting-value "language"))

;;(display (db-all db "local" "app-settings"))(newline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define earth-radius-metres 6371000)
(define (to-radians a) (* a (/ 3.141592653589793 180)))

(define (gps-distance a b)
  (let ((lata (to-radians (car a)))
        (lona (to-radians (cadr a)))
        (latb (to-radians (car b)))
        (lonb (to-radians (cadr b))))
    (let ((cosang  (+ (* (cos lata) (cos latb) (cos (- lonb lona)))
                      (* (sin lata) (sin latb)))))
      (* (acos cosang) earth-radius-metres))))

(define (clear-triggers)
  (list
   (delayed "when-moved" 200 (lambda () '()))
   (delayed "when-timer" 200 (lambda () '()))))

(define-macro (when-moved-metres . args)
  `(begin
     (define (when-moved-cb)
       (append
        (list
         (toast
          (string-append
           (number->string (car (get-current 'location '(0 0)))) ","
           (number->string (cadr (get-current 'location '(0 0)))) " : "
           (number->string (gps-distance (get-current 'location '(0 0))
                                         (get-current 'last-moved-location '(0 0)))))))
        (cond
         ((> (gps-distance (get-current 'location '(0 0))
                           (get-current 'last-moved-location '(0 0)))
             ,(car args))
          (set-current! 'last-moved-location (get-current 'location '(0 0)))
          (append ,@(cdr args)))
         (else
          '()))
        (list (delayed "when-moved" 2000 when-moved-cb))))
     (list
      (delayed "when-moved" 2000 when-moved-cb))))

(define-macro (when-timer . args)
  `(begin
     (define (when-timer-cb)
       (append
        ,@(cdr args)
        (list (delayed "when-timer" (* 1000 ,(car args)) when-timer-cb))))
     (list
      (delayed "when-timer" (* 1000 ,(car args)) when-timer-cb))))

(define (new-location? dist)
  (cond
   ((foldl ;; are we inside the radius of any previous location?
     (lambda (loc r)
       (cond
        ((and (not r)
              (< (gps-distance (get-current 'location '(0 0)) loc) dist))
         #t)
        (else r)))
     #f
     (get-current 'location-list '()))
    #f)
   (else
    (set-current! 'location-list (cons (get-current 'location '(0 0)) (get-current 'location-list '())))
    #t)))

(define-macro (when-in-new-location . args)
  `(begin
     (define (when-new-location-cb)
       (append
        (list
         (toast
          (string-append
           (number->string (car (get-current 'location '(0 0)))) ","
           (number->string (cadr (get-current 'location '(0 0)))) ": "
           (number->string (length (get-current 'location-list '()))))))
        (cond
         ((new-location? ,(car args))
          (set-current! 'last-moved-location (get-current 'location '(0 0)))
          (append ,@(cdr args)))
         (else
          '()))
        (list (delayed "when-new-location" 2000 when-new-location-cb))))
     ;; clear locations
     (set-current! 'location-list '())
     (list
      (delayed "when-new-location" 2000 when-new-location-cb))))


(define (noise)
  (list (play-sound "ping")))

(define (shake)
  (list (vibrate 200)))

(define (sensor->ktv-list d)
  (let ((name (car d)))
    (index-map
     (lambda (i d)
       (if (number? d)
           (ktv (string-append name "-" (number->string i)) "real" d)
           (ktv (string-append name "-" (number->string i)) "varchar" d)))
     (cadr d))))

(define (save-to-db name . data)
  (define photo-name "")
  (entity-create!
   db "stream" name
   (cons
    (ktv "name" "varchar" name)
    (foldl
     (lambda (i r)
       (cond
        ((not i) r)
        ((equal? (car i) "take-photo")
         ;; take a photo at the end...
         ;; only set the filename once, reuse it for multiple references
         (when (equal? photo-name "")
               (set! photo-name (cadr i)))
         (append r (list (ktv "photo" "file" photo-name))))
        (else
         (append r (sensor->ktv-list i)))))
     '()
     data)))
  (append
   (if (not (equal? photo-name ""))
       (begin
         (alog "sending take picture")
         (list (update-widget 'camera-preview (get-current 'camera-preview-id 0) 'take-picture-cont photo-name)))
       '())
   (list (toast (string-append "saved entity: " name)))))


(define (camera)
  (let ((t (time-of-day)))
    (list "take-photo" (string-append "files/photo-"
                                      (number->string (car t)) "-" (number->string (cadr t))
                                      ".jpg"))))

(define (take-photo)
  (let ((t (time-of-day)))
    (list "take-photo" (string-append "files/photo-"
                                      (number->string (car t)) "-" (number->string (cadr t))
                                      ".jpg"))))



(define (show . data)
  (list
   (if data
       (if (string? data)
           (toast-size data 10)
           (toast-size
            (foldl
             (lambda (d r)
               (if d
                   (string-append r " " (escape-quotes (scheme->json d)))
                   (string-append r " no data yet...")))
             "" data)
            10))
       (toast-size "no data yet..." 10))))

(define (get-sensor-value type)
  ;; add to the list of sensor if it's not there yet
  (set-current! 'sensors (set-add type (get-current 'sensors '())))
  ;; return a sensor type/data list
  (let ((item (assv type (get-current 'sensor-values '()))))
    (if item (list (sensor-type->string (car item)) (cadr item)) #f)))

(define (get-camera-property name)
  (let ((found (assoc name camera-properties)))
    (if found (cadr found) #f)))

(define (gps) (list "gps" (get-current 'location '())))

(define (accelerometer) (get-sensor-value sensor-accelerometer))
(define (ambient-temperature) (get-sensor-value sensor-ambient-temperature))
(define (game-rotation-vector) (get-sensor-value sensor-game-rotation-vector))
(define (geomagnetic-rotation-vector) (get-sensor-value sensor-geomagnetic-rotation-vector))
(define (gravity) (get-sensor-value sensor-gravity))
(define (gyroscope) (get-sensor-value sensor-gyroscope))
(define (gyroscope-uncalibrated) (get-sensor-value sensor-gyroscope-uncalibrated))
(define (light) (get-sensor-value sensor-light))
(define (heart-rate) (get-sensor-value sensor-heart-rate))
(define (linear-acceleration) (get-sensor-value sensor-linear-acceleration))
(define (magnetic-field) (get-sensor-value sensor-magnetic-field))
(define (magnetic-field-uncalibrated) (get-sensor-value sensor-magnetic-field-uncalibrated))
(define (orientation) (get-sensor-value sensor-orientation))
(define (pressure) (get-sensor-value sensor-pressure))
(define (proximity) (get-sensor-value sensor-pressure))
(define (relative-humidity) (get-sensor-value sensor-relative-humidity))
(define (rotation-vector) (get-sensor-value sensor-rotation-vector))
(define (significant-motion) (get-sensor-value sensor-significant-motion))
(define (step-counter) (get-sensor-value sensor-step-counter))
(define (step-detector) (get-sensor-value sensor-step-detector))

(define (sensor-type->string s)
  (cond
   ((equal? s sensor-accelerometer) "accelerometer")
   ((equal? s sensor-ambient-temperature) "ambient-temperature")
   ((equal? s sensor-game-rotation-vector) "game-rotation-vector")
   ((equal? s sensor-geomagnetic-rotation-vector) "geomagnetic-rotation-vector")
   ((equal? s sensor-gravity) "gravity")
   ((equal? s sensor-gyroscope) "gyroscope")
   ((equal? s sensor-gyroscope-uncalibrated) "gyroscope-uncalibrated")
   ((equal? s sensor-heart-rate) "heart-rate")
   ((equal? s sensor-light) "light")
   ((equal? s sensor-linear-acceleration) "linear-acceleration")
   ((equal? s sensor-magnetic-field) "magnetic-field")
   ((equal? s sensor-magnetic-field-uncalibrated) "magnetic-field-uncalibrated")
   ((equal? s sensor-orientation) "orientation")
   ((equal? s sensor-pressure) "pressure")
   ((equal? s sensor-proximity) "proximity")
   ((equal? s sensor-step-counter) "step-counter")
   ((equal? s sensor-step-detector) "step-detector")
   (else "unknown-sensor")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define did 100)
(define (new-id)
  (set! did (+ did 1))
  (number->string did))

(define (inner-code-blockify code)
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
  (let ((code (json/parse-string text)))
    (map inner-code-blockify code)))

(define (load-code)
  (text->code-block (entity-get-value "text")))

(define (code-block text children)
  (cond
   ;; dispatch to special forms
   ((equal? text "text") (text-code-block text '()))
   ((equal? text "number") (number-code-block 0))
   (else
    (let ((id (new-id)))
      (draggable
       (make-id (string-append id "-code-block"))
       'vertical wrap (code-block-colour text)
       (if (code-block-atom? text) "drag-only" "normal")
       (append
        (list
         (horiz (text-view 0 text 30 wrap)
                (space (layout 40 40 1 'centre 0))
                (button (make-id (string-append id "-code-block-help"))
                        "?" 20 (layout 40 40 -1 'centre 0)
                        (lambda ()
                          (list (ok-dialog "help"
                                           (cadr (assoc text dox))
                                           (lambda () '())))))))
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
      (horiz
       (edit-text (make-id (string-append id "-edit"))
                  (number->string num) 30 "numeric" wrap
                  (lambda (v)
                    ;; clearly this is dubious, but it works...!
                    (set! num (string->number v))
                    '()))
       (space (layout 40 40 1 'centre 0))))
     (lambda ()
       (scheme->json (list 1 num))))))

(define (text-code-block text)
  (let ((id (new-id)))
    (draggable
     (make-id (string-append id "-code-block"))
     'vertical wrap (list 255 255 255 255)
     "drag-only"
     (list
      (horiz
       (edit-text (make-id (string-append id "-edit"))
                  text 30 "normal" wrap
                  (lambda (v)
                    ;; clearly this is dubious, but it works...!
                    (set! text v)
                    '()))
       (space (layout 40 40 1 'centre 0))))
     (lambda ()
       (scheme->json (list 1 (string-append "\\\"" text "\\\"")))))))


;; start with some default sensors
(set-current! 'sensors (list
                        sensor-accelerometer
                        sensor-gravity
                        sensor-gyroscope
                        sensor-magnetic-field
                        sensor-orientation))

;; top level eval
(define (eval-blocks t)
  (append
   (foldl (lambda (sexp r)
            (append (eval sexp) r)) '() t)
   (list
    (sensors-start
     "start-sensors"
     (get-current 'sensors '())
     (lambda (data)
       (set-current! 'sensor-values
                     (addv
                      (get-current 'sensor-values '())
                      (list (list-ref data 1)
                            (cdr (cdr (cdr (cdr data)))))))
       '())))))

(define dox
  (list
   (list "when-timer" "Triggers actions (green blocks) added to this block, the top item should be a number - the period in seconds to trigger things.")
   (list "when-moved-metres"  "Triggers actions (green blocks) added to this block using GPS, the top item should be a number - the distance in metres to trigger things.")
   (list "when-in-new-location" "The same as when-moved-metres, but triggers actions based on distance in metres to all previous trigger locations. The top item should be a number - the distance in metres.")
   (list "show" "Display all the values contained by this block - for testing e.g. sensor data.")
   (list "noise" "Play a sound, useful for indication of actions while the screen is turned off.")
   (list "shake" "Fire the vibration motor for a short time, useful for indication of actions while the screen is turned off - but don't do this while taking pictures :)")
   (list "save-to-db" "Saves all the data contained in this block (sensors or take-picture etc) as a database record. The top item needs to be a text block with the name to give the record type (can be anything).")
   (list "accelerometer" "Returns the data from your accelerometer sensor, takes no other blocks.")
   (list "ambient-temperature" "Returns the data from your temperature sensor, takes no other blocks.")
   (list "game-rotation-vector" "Returns rotation data (fast mode), takes no other blocks.")
   (list "geomagnetic-rotation-vector" "Returns geomagnetic rotation data (fast mode), takes no other blocks.")
   (list "gravity" "Returns the data from the gravity sensor, takes no other blocks.")
   (list "gyroscope" "Returns rotation data from the gyroscope sensor, takes no other blocks.")
   (list "gyroscope-uncalibrated" "Uncalibrated data from the gyroscope, takes no other blocks.")
   (list "light" "Returns the data from the light sensor, takes no other blocks.")
   (list "heart-rate" "Returns the data from the heart rate sensor, takes no other blocks.")
   (list "linear-acceleration" "Returns the data from the accelerometer sensor, takes no other blocks.")
   (list "magnetic-field" "Returns the data from the magnetic field sensor (direction of 'north'), takes no other blocks.")
   (list "magnetic-field-uncalibrated" "Returns the uncalibrated data from the magnetic field sensor, takes no other blocks.")
   (list "orientation" "Returns orientation calculated from combination of other sensors, takes no other blocks.")
   (list "pressure" "Returns the data from the air pressure sensor, takes no other blocks.")
   (list "proximity" "Returns the data from the proximity sensor (used to turn off the screen when your head is close), takes no other blocks.")
   (list "relative-humidity" "Returns the data from the relative humidity sensor, takes no other blocks.")
   (list "rotation-vector" "Returns the data from the rotation-vector sensor (slow mode), takes no other blocks.")
   (list "significant-motion" "Returns the data from the significant motion sensor, takes no other blocks.")
   (list "step-counter" "Returns the data from the step counter sensor, takes no other blocks.")
   (list "step-detector" "Returns the data from the step detector sensor, takes no other blocks.")
   (list "gps" "Returns the GPS latitude/longitude data, takes no other blocks.")
   (list "take-photo" "Triggers the camera and returns the filename. The image is stored in sdcard/uavtools/files. Put in a save-to-db block with sensors to link photos with sensor data.")
   (list "camera-horiz-angle" "The horizontal viewing angle of the camera")
   (list "camera-vert-angle" "The vertical viewing angle of the camera")
   (list "text" "Allows you to enter free text (for names of database record types)")
   (list "number" "Allows you to enter numbers")
   (list "+" "Useful for adding things together. Takes any number of blocks.")
   (list "-" "Useful for taking numbers away from each other. Takes any number of blocks.")
   (list "/" "Useful for dividing a number by another one. Takes any number of blocks.")
   (list "*" "Useful for multiplying numbers together. Takes any number of blocks.")
   (list "sin" "Mathematical sine function. Takes one number block in radians.")
   (list "cos" "Mathematical cosine function. Takes one number block in radians.")
   (list "tan" "Mathematical tangent function. Takes one number block in radians.")
   (list "asin" "Arc sine function. Takes one number block.")
   (list "acos" "Arc cosine function. Takes one number block.")
   (list "atan" "Arc tangent function. Takes one number block.")
   (list "modulo" "Modulus of one number by another. Takes two number blocks.")
   (list "pow" "Mathematical power function, takes two number blocks.")
   (list "to-radians" "Converts degrees to radians. Takes one number block.")
  ))

(define trigger-colour (list 255 200 100 255))
(define trigger-functions
  (list "when-timer" "when-moved-metres" "when-in-new-location"))

(define action-colour (list 200 255 100 255))
(define action-functions
  (list "show" "noise" "shake" "save-to-db"))

(define sensor-colour (list 255 100 200 255))
(define sensor-functions
  (list
  "accelerometer"
  "ambient-temperature"
  "game-rotation-vector"
  "geomagnetic-rotation-vector"
  "gravity"
  "gyroscope"
  "gyroscope-uncalibrated"
  "light"
  "heart-rate"
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
  "take-photo"
  "camera-horiz-angle"
  "camera-vert-angle"
  "step-counter"
  "step-detector"))

(define (camera-horiz-angle)
  (get-camera-property 'horis-angle)) ;; sic

(define (camera-vert-angle)
  (let ((v (get-camera-property 'vert-angle)))
    v))

(define maths-colour (list 200 100 255 255))
(define maths-functions
  (list "text" "number" "+" "-" "/" "*" "sin" "cos" "tan" "asin" "acos" "atan" "modulo" "pow" "to-radians"))

(define (code-block-colour text)
  (cond
   ((string-in-list-fast text trigger-functions) trigger-colour)
   ((string-in-list-fast text action-functions) action-colour)
   ((string-in-list-fast text sensor-functions) sensor-colour)
   ((string-in-list-fast text maths-functions) maths-colour)
   (else (list 255 0 255 255))))

(define (code-block-known? text)
  (or
   (string-in-list-fast text trigger-functions)
   (string-in-list-fast text action-functions)
   (string-in-list-fast text sensor-functions)
   (string-in-list-fast text maths-functions)))

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
                    'contents-add (list (code-block fn '())))))))
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
        (mbutton-scale 'triggers (lambda () (build-menu trigger-functions)))
        (mbutton-scale 'actions (lambda () (build-menu action-functions)))
        (mbutton-scale 'sensors (lambda () (build-menu sensor-functions)))
        (mbutton-scale 'maths (lambda () (build-menu maths-functions)))
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
    (text-view (make-id "version") (string-append "beta/experimental " (number->string app-version)) 20 fillwrap)

    (scroll-view-vert
     0 (layout 'fill-parent 'wrap-content 0.75 'centre 0)
     (list
      (linear-layout
       (make-id "button-list") 'vertical
       (layout 'fill-parent 'wrap-content 0.75 'centre 0)
       (list 0 0 0 0)
       (list
     (button
      (make-id "about-button")
      "About"
      30 (layout 'fill-parent 'wrap-content -1 'centre 5)
      (lambda ()
        (list (start-activity "about" 0 ""))))

     (build-list-widget db "code" 'programs (list "name") "program" "vptest"
                        (lambda () #f)
                        (lambda ()
                          (list
                           (ktv "name" "varchar" "a program")
                           (ktv "text" "varchar" (scheme->json '((when-timer 3 (show "hello world")))))))
                        )

     (button
      (make-id "review")
      "View data"
      30 (layout 'fill-parent 'wrap-content -1 'centre 5)
      (lambda ()
        (list (start-activity "review" 0 ""))))

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

     ))))
    )

   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (list
      ;; start gps here, and run it all the time...
      (gps-start "gps" (lambda (loc)
                         (set-current! 'location loc)
                         (list))
                 500 5)

      (update-list-widget db "code" (list "name") "program" "vptest" #f)
      ))
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
          (list
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
           sensor-significant-motion)
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
                              (text-view (make-id (string-append (list-ref sensor 0) "-values"))
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

  (vert
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

      (mtoggle-button-scale
       'eval
       (lambda (v)
         (if (eqv? v 1)
             (list
              (walk-draggable
               "eval-walk" (get-id "block-root")
               (lambda (t)
                 (eval-blocks t))))
             (clear-triggers))))


      (mbutton-scale
       'save
       (lambda ()
         (list
          (walk-draggable
           "eval-walk" (get-id "block-root")
           (lambda (t)
             (entity-set-value! "text" "varchar" (scheme->json t))
             (entity-update-values!)
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
      (medit-text 'program-name "normal" (lambda (v) (entity-set-value! "name" "varchar" v) '()))

      (scroll-view-vert
       0 (layout 'fill-parent 'fill-parent 1 'centre 0)
       (list
        (vert
        (draggable
         (make-id "block-root")
         'vertical (layout 'fill-parent 'fill-parent 1 'left 0) (list 255 255 0 20)
         "drop-only"
         (list)
         (lambda ()
           (scheme->json (list 0 ""))))

        (horiz
         (button (make-id "lock-button")
                 "flight lock" 30 (layout 'fill-parent 'wrap-content 1 'centre 5)
                 (lambda ()
                   (list
                    (alert-dialog
                     "vptest-lock"
                     "Enter flight mode: phone needs hard reset to stop the program - are you sure?"
                     (lambda (v)
                       (cond
                        ((eqv? v 1)
                         (list
                          (update-widget 'camera-preview (get-id "camerap") 'shutdown 0)
                          (start-activity "lock" 0 "")))
                        (else
                         (list))))))))

         (delete-button)

         (button (make-id "log-button")
                 "error log" 30 (layout 'fill-parent 'wrap-content 1 'centre 5)
                 (lambda ()
                   (list
                    (start-activity "log" 0 ""))))

         )

        (camera-preview (make-id "camerap") (layout 'fill-parent 320 1 'left 0)))


        )))))


   (relative
    '(("parent-bottom"))
    (list 0 0 0 0)
     (vert

      (draggable
       (make-id "block-bin")
       'vertical (layout 'fill-parent 'wrap-content 1 'left 10) (list 255 255 0 20)
       "drop-only-consume"
       (list (mtext 'rubbish-bin))
       (lambda ()
         (scheme->json (list 0 ""))))))

   )
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (entity-init! db "code" "program" (get-entity-by-unique db "code" arg))
     (set-current! 'camera-preview-id (get-id "camerap"))
     (list
      (mupdate 'edit-text 'program-name "name")
      (update-widget
       'draggable (get-id "block-root")
       'contents (load-code))))
   (lambda (activity)
     (list (update-widget 'camera-preview (get-id "camerap") 'shutdown 0)))
   (lambda (activity) '())
   (lambda (activity)
     (list (update-widget 'camera-preview (get-id "camerap") 'shutdown 0)))
   (lambda (activity)
     (list (update-widget 'camera-preview (get-id "camerap") 'shutdown 0)))
   (lambda (activity requestcode resultcode) '()))


  (activity
   "lock"
   (vert
    (button (make-id "exit-button")
            "Exit" 20 (layout 'fill-parent 'wrap-content 1 'left 5)
            (lambda ()
              (append
               (clear-triggers)
               (list
                ;; shut it all down
                (update-widget 'camera-preview (get-id "lock-camerap") 'shutdown 0)
                (finish-activity 1)))))

    (draggable
     (make-id "block-root")
     'vertical (layout 10 10 1 'left 0) (list 255 255 0 20)
     "drop-only"
     (list)
     (lambda ()
       (scheme->json (list 0 ""))))

    (camera-preview (make-id "lock-camerap") (layout 'fill-parent 320 1 'left 0))
    )
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (set-current! 'camera-preview-id (get-id "lock-camerap"))
     (list
      (update-widget
       'draggable (get-id "block-root")
       'contents (load-code))
      (walk-draggable
       "lock-eval-walk" (get-id "block-root")
       (lambda (t)
         (eval-blocks t)))))
   (lambda (activity)
     (list (update-widget 'camera-preview (get-id "lock-camerap") 'shutdown 0)))
   (lambda (activity) '())
   (lambda (activity)
     (list (update-widget 'camera-preview (get-id "lock-camerap") 'shutdown 0)))
   (lambda (activity)
     (list (update-widget 'camera-preview (get-id "lock-camerap") 'shutdown 0)))
   (lambda (activity requestcode resultcode) '()))

  (activity
   "review"
   (vert
    (horiz
     (text-view 0 "View data" 40 (layout 'fill-parent 'wrap-content 1 'left 0))
     (spinner (make-id "entity-type-spinner")
              '()
              (layout 'fill-parent 'wrap-content 1 'centre 0)
              (lambda (v)
                (review-update-list
                 (list-ref (map (lambda (i) (vector-ref i 0)) (get-all-entity-types db "stream")) v)))))


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
   (lambda (activity arg)
     (list
      (update-widget 'spinner (get-id "entity-type-spinner")
                     'array
                     (map (lambda (i) (vector-ref i 0))
                          (get-all-entity-types db "stream")))))
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

  (activity
   "log"
   (scroll-view-vert
    0 (layout 'fill-parent 'wrap-content 1 'left 0)
    (list
     (vert
      (text-view-left (make-id "log-view") "" 20 (layout 'fill-parent 'fill-parent 1 'left 0))
      (button (make-id "exit-button")
              "Exit" 20 (layout 'fill-parent 'wrap-content 1 'left 5)
              (lambda ()
                (list (finish-activity 1)))))))
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (list
      (update-widget 'text-view (get-id "log-view") 'file "sdcard/jellyfish-log.txt")))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))


  (activity
   "about"

   (scroll-view-vert
    0 (layout 'fill-parent 'wrap-content 1 'centre 0)
    (list
     (vert
      (image-view 0 "logo" (layout 300 'fill-parent 1 'centre 0))
      (text-view
       0
       "The UAV toolkit is an experimental application for making use of your smartphone's sensors for airborne science. It's main purpose is using time or space based triggers to capture images with associated sensor data for further processing."
       20 (layout 'fill-parent 'wrap-content -1 'centre 10))

      (text-view 0 "Credits" 30 (layout 'fill-parent 'wrap-content -1 'centre 5) 'centre)

      (text-view 0 "Karen Anderson: team leader and chief kite wranger" 20 (layout 'fill-parent 'wrap-content -1 'centre 0))
      (text-view 0 "Steve Hancock: extreme flight testing and duct tape" 20 (layout 'fill-parent 'wrap-content -1 'centre 0))
      (text-view 0 "James Duffy: drone test pilot and precise landings" 20 (layout 'fill-parent 'wrap-content -1 'centre 0))
      (text-view 0 "Leon DeBell:  3d printing and drone mechanics" 20 (layout 'fill-parent 'wrap-content -1 'centre 0))
      (text-view 0 "Liam Reinhardt: kite handler and knot consultant" 20 (layout 'fill-parent 'wrap-content -1 'centre 0))
      (text-view 0 "Dave Griffiths: code monkey and software design" 20 (layout 'fill-parent 'wrap-content -1 'centre 0))
      (spacer 20)
      (text-view 0 "Free/Open Source software assembled in Cornwall by Foam Kernow" 20 (layout 'fill-parent 'wrap-content -1 'centre 10))
      (image-view 0 "foam" (layout 'wrap-content 'wrap-content -1 'centre 0))

      (spacer 20)
      (text-view 0 "Supported by" 20 (layout 'fill-parent 'wrap-content -1 'centre 10))
      (image-view 0 "exeter" (layout 'wrap-content 'fill-parent -1 'centre 0))
      (text-view 0 "Environment and Sustainability Institute" 20 (layout 'fill-parent 'wrap-content -1 'centre 10))

      (spacer 20)
      (button (make-id "about-back-button")
              "Back" 20 (layout 'fill-parent 'wrap-content 1 'left 5)
              (lambda ()
                (list (finish-activity 1))))

      )))
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))




  )


;(build-test! db "sync" village-ktvlist household-ktvlist individual-ktvlist)
