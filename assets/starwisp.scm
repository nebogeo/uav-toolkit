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
(define entity-types (list "village" "household" "individual" "child" "crop"))

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

(define code-version 2)

(insert-entity-if-not-exists
 db "code" "program" "null" code-version
 (list
  (ktv "text" "varchar" (scheme->json '(toast (number->string (+ 2 3 4)))))))

(set-current! 'user-id (get-setting-value "user-id"))
(set! i18n-lang (get-setting-value "language"))

;;(display (db-all db "local" "app-settings"))(newline)

(define did 100)
(define (new-id)
  (set! did (+ did 1))
  (number->string did))

(define (inner-code-blockify code)
  (msg "inner:" code)
  (cond
   ((null? code) (code-block "list" '()))
   ((number? code) (code-block (number->string code) '()))
   ((symbol? code) (code-block (symbol->string code) '()))
   ((string? code) (code-block code '()))
   ((list? code)
    ;; ignore first 'list'
    (code-block (cadr code)
                (map inner-code-blockify (cddr code))))
   (else (code-block "error" '()))))

(define (text->code-block text)
  (msg "text->codeblock" text)
  (let ((code (json/parse-string text)))
    (inner-code-blockify code)))

(define (load-code)
  (text->code-block (ktv-get (get-entity db "code" code-version) "text")))

(define (code-block text children)
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
       (msg "code-block callback called")
       text))))

(define control-colour (list 255 200 100 255))
(define control-functions
  (list "when-timer" "when-moved-metres" "when-update" "when-pressed"))

(define data-colour (list 200 255 100 255))
(define data-functions
  (list "save-entity" "filter" "load-entity"))

(define sensor-colour (list 255 100 200 255))
(define sensor-functions
  (list "accelerometer" "gps" "camera" "orientation" "air-pressure" "light" "proximity" "altitude" "camera-horiz-angle" "camera-vert-angle"))

(define maths-colour (list 200 100 255 255))
(define maths-functions
  (list "+" "-" "/" "*" "sin" "cos" "tan" "asin" "acos" "atan" "modulo" "pow" "300" "hello world"))

(define display-colour (list 100 255 200 255))
(define display-functions
  (list "toast" "sound"))

(define (code-block-colour text)
  (cond
   ((string-in-list-fast text control-functions) control-colour)
   ((string-in-list-fast text data-functions) data-colour)
   ((string-in-list-fast text sensor-functions) sensor-colour)
   ((string-in-list-fast text maths-functions) maths-colour)
   ((string-in-list-fast text display-functions) display-colour)
   (else (list 255 0 255 255))))

(define (code-block-atom? text)
  (string-in-list-fast text (append '("300") sensor-functions)))


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
         (walk-draggable "eval-walk" (get-id "block-root")
                         (lambda (t)
                           (msg t)
                           (msg (eval t))

                           (list
                            (eval t)
                            ))))))
     (mbutton-scale
      'save
      (lambda ()
        (list
         (walk-draggable "eval-walk" (get-id "block-root")
                         (lambda (t)
                           (update-entity
                            db "code" code-version
                            (list
                             (ktv "text" "varchar" (scheme->json t))))
                           (list
                            (toast (string-append "saved"))))))))
     )

    (build-fragment
     "" (make-id "menu-holder")
     (layout 'fill-parent 'wrap-content -1 'left 0))

    (spacer 20)

    (scroll-view-vert
     0 (layout 'fill-parent 'fill-parent -1 'centre 0)
     (list
      (draggable
       (make-id "block-root")
       'vertical (layout 'fill-parent 'fill-parent -1 'left 0) (list 255 255 0 20)
       "drop-only"
       (list)
       (lambda () "")))))
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (list
      (update-widget
       'draggable (get-id "block-root")
       'contents (list (load-code)))))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode)
     (list)))


  )


;(build-test! db "sync" village-ktvlist household-ktvlist individual-ktvlist)
