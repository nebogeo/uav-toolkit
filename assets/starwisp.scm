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
     'vertical wrap (list 120 255 120 50)
     (append
      (list
       (text-view 0 text 30 wrap))
      children)
     (lambda ()
       (msg "code-block callback called")
       text))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fragments


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
     (mbutton-scale
      'add-block
      (lambda ()
        (list (update-widget
               'draggable 99
               'contents (list (code-block "foo" '()))))))
     (mbutton-scale
      'eval
      (lambda ()
        (list
         (walk-draggable "eval-walk" 99
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
         (walk-draggable "eval-walk" 99
                         (lambda (t)
                           (update-entity
                            db "code" code-version
                            (list
                             (ktv "text" "varchar" (scheme->json t))))
                           (list
                            (toast (string-append "saved"))))))))
     )
    (scroll-view-vert
     0 (layout 'fill-parent 'wrap-content 1 'centre 0)
     (list
      (draggable
       99 'vertical (layout 'fill-parent 'fill-parent 1 'left 0) (list 255 255 255 255)
       (list
;        (code-block "toast" 20
;                    (list
;                     (code-block "number->string" 20
;                                 (list
;                                  (code-block "+" 20
;                                              (list
;                                               (code-block "3" 20 '())
;                                               (code-block "4" 20 '())))
;                                  ))))

;        (code-block "toast" 20
;                    (list
;                     (code-block "string-append" 20
;                                 (list
;                                  (code-block "symbol->string" 20 (list (code-block "'hello" 20 '())))
;                                  (code-block "symbol->string" 20 (list (code-block "'world" 20 '()))))
;                     )))


        ;            (list
        ;             (code-block "2" "hello world" 20 '())))
        )
       (lambda () "")))))
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (list
      (update-widget
       'draggable 99
       'contents (list (load-code)))
      ))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode)
     (list)))


  )


;(build-test! db "sync" village-ktvlist household-ktvlist individual-ktvlist)
