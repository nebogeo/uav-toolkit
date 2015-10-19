;; UAV toolkit Copyright (C) 2015 Foam Kernow
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

(define settings-entity-id-version 4)

(define (insert-if-not-exists-name db table type name ktv-list)
  (when
   (null? (filter-entities-inc-deleted db table type (list (list "name" "varchar" "=" name))))
   (insert-entity db table type "sys" ktv-list)))

 (insert-entity-if-not-exists
  db "local" "app-settings" "null" settings-entity-id-version
  (list
   (ktv "language" "int" 0)
   (ktv "alt" "real" "50.0")
   (ktv "coverage" "real" "60.0")
   (ktv "timer" "real" "3.0")))

(define (get-setting-value name)
  (ktv-get (get-entity db "local" settings-entity-id-version) name))

 (define (set-setting! key type value)
   (update-entity
    db "local" settings-entity-id-version (list (ktv key type value))))

;; need access via normal scheme code
(define global-altitude (get-setting-value "altitude"))
(define global-coverage (get-setting-value "coverage"))
(define global-timer (get-setting-value "timer"))

(define (default-code name code)
  (insert-if-not-exists-name
   db "code" "program" name
   (list
    (ktv "name" "varchar" name)
    (ktv "text" "varchar" (json/gen-string code)))))

(define (default-func name code)
  (insert-if-not-exists-name
   db "code" "function" name
   (list
    (ktv "name" "varchar" name)
    (ktv "text" "varchar" (json/gen-string code)))))

;;; default code stuff

(default-func "jerk"
  '((lambda () (let ((ret (- (list-mag (cadr (accelerometer))) (get-current 'jerk-value 0)))) (set-current! 'jerk-value (list-mag (cadr (accelerometer)))) ret))))

(default-func "jerk-falloff"
  '((lambda (f) (set-current! 'jerk-f-val (* (+ (jerk) (get-current 'jerk-f-val 0)) f)) (get-current 'jerk-f-val 0))))

(default-func "tilt"
  '((lambda (arg) (and (between (sensor-value (orientation) 1) (* -1 arg) arg) (between (sensor-value (orientation) 2) (* -1 arg) arg)))))

(default-func "cam-angle-to-distance"
  '((lambda (altitude coverage)
      (* (tan (to-radians (/ (camera-vert-angle) 2)))
         (* altitude (/ coverage 100))))))

(default-func "jerktilt test"
  '((when-timer
     0.5
     (when
      (and
       (< (jerk-falloff 0.5) 2)
       (tilt 0.5))
      (noise)))))

(default-code "Timed Drone"
  '((when-timer
     global-timer
     (save-to-db
      "timed-drone"
      (orientation)
      (gyroscope)
      (gravity)
      (accelerometer)
      (magnetic-field)
      (gps)
      (take-photo))
     (noise))))

(default-code "Clever Drone"
  '((when-in-new-location
     (cam-angle-to-distance
      global-altitude global-coverage)
     (save-to-db "clever-drone"
                 (orientation)
                 (gyroscope)
                 (gravity)
                 (accelerometer)
                 (magnetic-field)
                 (gps)
                 (take-photo))
     (noise))))

(default-code "Timed Kite"
  '((when-timer
     global-timer
     (when
      (and
       (< (jerk-falloff 0.5) 2)
       (tilt 0.5))
      (append
       (save-to-db
        "timed-kite"
        (orientation)
        (gyroscope)
        (gravity)
        (accelerometer)
        (magnetic-field)
        (gps)
        (take-photo))
       (noise))))))

(default-code "Clever Kite"
  '((when-in-new-location
     (cam-angle-to-distance global-altitude global-coverage)
     (when
      (and
       (< (jerk-falloff 0.5) 2)
       (tilt 0.5))
      (append
       (save-to-db "clever-kite"
                   (orientation)
                   (gyroscope)
                   (gravity)
                   (accelerometer)
                   (magnetic-field)
                   (gps)
                   (take-photo))
       (noise))))))

;;; end default code stuff


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

(define (sensor-value s i) (list-ref (cadr s) i))

(define (between v l h)
  (and (> v l) (<= v h)))

(define (list-mag l)
  (msg l)
  (foldl
   (lambda (i r)
     (+ (* i i) r))
   0 l))



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
  (msg (length data))
  (msg data)
  (string? (car data))
  (list
   (if data
       (if (string? data)
           (toast-size data 20)
           (toast-size
            (foldl
             (lambda (d r)
               (if d
                   (string-append r " " (escape-quotes (json/gen-string d)))
                   (string-append r " no data yet...")))
             "" data)
            10))
       (toast-size "no data yet..." 10))))

;; returns (sensor-type (value0 value1 ...))
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
   ((null? code) (code-block-list '()))
   ((number? code) (number-code-block code))
   ((string? code) (text-code-block code))
   ((symbol? code)
    (if (or (not (code-block-known? code))
            (eqv? code 'symbol))
        (symbol-code-block code '())
        (code-block code '())))
   ((list? code)
    (if (symbol? (car code));; convert into function call
        (code-block (car code)
                    (map inner-code-blockify (cdr code)))
        (code-block-list (map inner-code-blockify code))))
   (else (code-block 'error '()))))

(define (text->code-block text)
  (msg "text->code-block" text)
  (let ((code (json/parse-string text)))
    (msg "&convert" code)
    (map inner-code-blockify code)))

(define (load-code)
  (text->code-block (entity-get-value "text")))

(define (code-block text children)
  (cond
   ;; dispatch to special forms
   ((eq? text 'text) (text-code-block text '()))
   ((eq? text 'number) (number-code-block 0))
   ((eq? text 'symbol) (symbol-code-block 'symbol))
   ((eq? text 'empty) (code-block-list '()))
   (else
    (let ((id (new-id)))
      (draggable
       (make-id (string-append id "-code-block"))
       'vertical wrap (code-block-colour text)
       "normal"
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
         (json/gen-string
          (list 0 text))))))))

(define (code-block-list children)
  (let ((id (new-id)))
    (draggable
     (make-id (string-append id "-code-block"))
     'vertical wrap '(255 255 255 100)
     "normal"
     (cons
      (space (layout 40 40 1 'centre 0))
      children)
     (lambda ()
       (json/gen-string
        (list 0 ""))))))


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
       (json/gen-string (list 2 num))))))

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
       ;(json/gen-string (list 1 (string-append "\\\"" text "\\\"")))))))
       (json/gen-string (list 1 text))))))

(define (symbol-code-block sym)
  (let ((id (new-id))
        (text (symbol->string sym)))
    (draggable
     (make-id (string-append id "-code-block"))
     'vertical wrap (list 255 100 100 255)
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
       (json/gen-string (list 1 (string->symbol text)))))))


;; start with some default sensors
(set-current! 'sensors (list
                        sensor-accelerometer
                        sensor-gravity
                        sensor-gyroscope
                        sensor-magnetic-field
                        sensor-orientation))

(define (eval-library)
  (set-current! 'library-fns '())
  (for-each
   (lambda (entity-id)
     (let ((entity (get-entity db "code" entity-id)))
       (let ((name (ktv-get entity "name"))
             (sexpr (json/parse-string (ktv-get entity "text"))))

         ;; add to list of functions for the library menu
         (set-current! 'library-fns
                       (cons name (get-current 'library-fns '())))

         ;; eval in global environment
         (eval (dbg (list
                     'define (string->symbol name)
                     (car sexpr)))
               (interaction-environment))
         )))
   (all-entities db "code" "function")))

(define (find-program-code name)
  (db-filter db "code" "program" (list (list "name" "varchar" "like" name))))

(define (eval-program text)
  (let ((sexpr (json/parse-string text)))
    ;; eval in global environment
   (foldl (lambda (sexp r)
            (append (eval sexp) r))
          '()
          sexpr)))

(define (find-library-code name)
  (db-filter db "code" "function" (list (list "name" "varchar" "like" name))))

;; top level eval
(define (eval-blocks t)
  (append
   (foldl (lambda (sexp r)
            (append (eval sexp) r))
          '()
          t)
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

(define code-colour (list 100 200 255 255))
(define code-functions
  (list "text" "number" "symbol" "empty"
        "when" "and" "or" "not" "lambda" "map" "foldl"
        "set-current!" "get-current" ))

(define trigger-colour (list 255 200 100 255))
(define trigger-functions
  (list "when-timer" "when-moved-metres" "when-in-new-location"
        "global-altitude" "global-coverage" "global-timer"))

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

(define library-colour (list 100 255 200 255))


(define (camera-horiz-angle)
  (get-camera-property 'horis-angle)) ;; sic

(define (camera-vert-angle)
  (let ((v (get-camera-property 'vert-angle)))
    v))

(define maths-colour (list 200 100 255 255))
(define maths-functions
  (list "+" "-" "/" "*" "<" ">" ">=" "<="
        "sin" "cos" "tan" "asin" "acos" "atan" "modulo" "pow"
        "to-radians" "between" "sensor-value" "list-mag"))

(define (code-block-colour text)
  (let ((text (symbol->string text)))
    (cond
     ((string-in-list-fast text code-functions) code-colour)
     ((string-in-list-fast text trigger-functions) trigger-colour)
     ((string-in-list-fast text action-functions) action-colour)
     ((string-in-list-fast text sensor-functions) sensor-colour)
     ((string-in-list-fast text maths-functions) maths-colour)
     ((string-in-list-fast text (get-current 'library-fns '())) library-colour)
     (else (list 255 0 255 255)))))

(define (code-block-known? text)
  (let ((text (symbol->string text)))
    (or
     (string-in-list-fast text code-functions)
     (string-in-list-fast text trigger-functions)
     (string-in-list-fast text action-functions)
     (string-in-list-fast text sensor-functions)
     (string-in-list-fast text maths-functions)
     (string-in-list-fast text (get-current 'library-fns '())))))

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
                    'contents-add (list (code-block (string->symbol fn) '())))))))
      fns)))
   (map
    (lambda (fn)
      (update-widget 'button (get-id (string-append fn "-button"))
                     'background-colour (code-block-colour (string->symbol (car fns)))))
    fns)
   ))
