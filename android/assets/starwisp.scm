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
        (mbutton-scale 'code (lambda () (build-menu code-functions)))
        (mbutton-scale 'triggers (lambda () (build-menu trigger-functions)))
        (mbutton-scale 'actions (lambda () (build-menu action-functions)))
        (mbutton-scale 'sensors (lambda () (build-menu sensor-functions)))
        (mbutton-scale 'maths (lambda () (build-menu maths-functions)))
        (mbutton-scale 'library (lambda () (build-menu (get-current 'library-fns '()))))
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
     (list
      (update-widget 'button (get-id "code") 'background-colour code-colour)
      (update-widget 'button (get-id "triggers") 'background-colour trigger-colour)
      (update-widget 'button (get-id "sensors") 'background-colour sensor-colour)
      (update-widget 'button (get-id "maths") 'background-colour maths-colour)
      (update-widget 'button (get-id "actions") 'background-colour action-colour)
      (update-widget 'button (get-id "library") 'background-colour library-colour)
      ))
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

        (mtitle 'global-settings)

        (medit-text 'global-alt "numeric"
                    (lambda (v)
                      (set-setting! "alt" "real" (string->number v))
                      '()))
        (medit-text 'global-cov "numeric"
                    (lambda (v)
                      (set-setting! "coverage" "real" (string->number v))
                      '()))
        (medit-text 'global-timer "numeric"
                    (lambda (v)
                      (set-setting! "timer" "real" (string->number v))
                      '()))


        (build-list-widget-readonly
         db "code" 'modes (list "name") "program" "vptest"
         (lambda () #f)
         (lambda ()
           (list
            (ktv "name" "varchar" "a program")
            (ktv "text" "varchar"
                 (json/gen-string '((when-timer 3 (show "hello world")))))))
         )



        (button
         (make-id "edit-but")
         "Modify code"
         20 (layout 'wrap-content 'wrap-content -1 'right 5)
         (lambda ()
           (list (start-activity "editor" 0 ""))))
     )))))
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     ;; run library code here...
     (list
      (update-widget 'edit-text (get-id "global-alt") 'text
                     (number->string (get-setting-value "alt")))
      (update-widget 'edit-text (get-id "global-cov") 'text
                     (number->string (get-setting-value "coverage")))
      (update-widget 'edit-text (get-id "global-timer") 'text
                     (number->string (get-setting-value "timer")))

      ;; start gps here, and run it all the time...
      (gps-start "gps" (lambda (loc)
                         (set-current! 'location loc)
                         (list))
                 500 5)
      (update-list-widget db "code" (list "name") "program" "lock" #f)

      ))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))


  (activity
   "editor"
   (vert

    (scroll-view-vert
     0 (layout 'fill-parent 'wrap-content 0.75 'centre 0)
     (list
      (linear-layout
       (make-id "button-list") 'vertical
       (layout 'fill-parent 'wrap-content 0.75 'centre 0)
       (list 0 0 0 0)
       (list

     (build-list-widget db "code" 'programs (list "name") "program" "vptest"
                        (lambda () #f)
                        (lambda ()
                          (list
                           (ktv "name" "varchar" "a program")
                           (ktv "text" "varchar" (json/gen-string '((when-timer 3 (show "hello world")))))))
                        )


     (button
      (make-id "library-but")
      "Code library"
      30 (layout 'fill-parent 'wrap-content -1 'centre 5)
      (lambda ()
        (list (start-activity "library" 0 ""))))

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
   "library"
   (vert
    (scroll-view-vert
     0 (layout 'fill-parent 'wrap-content 0.75 'centre 0)
     (list
      (linear-layout
       (make-id "button-list") 'vertical
       (layout 'fill-parent 'wrap-content 0.75 'centre 0)
       (list 0 0 0 0)
       (list
        (text-view (make-id "library-title") "Code that can be used by other programs" 20 fillwrap)

        (build-list-widget db "code" 'functions (list "name") "function" "vptest"
                           (lambda () #f)
                           (lambda ()
                             (list
                              (ktv "name" "varchar" "a program")
                              (ktv "text" "varchar" (json/gen-string '((when-timer 3 (show "hello world")))))))
                           )))))

    )
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (list
      (update-list-widget db "code" (list "name") "function" "vptest" #f)
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
             (msg "saving" t)
             (msg "&convert" (json/gen-string t))
             (entity-set-value! "text" "varchar" (json/gen-string t))
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
           (json/gen-string (list 0 ""))))

        (horiz
         ;; (button (make-id "lock-button")
         ;;         "flight lock" 30 (layout 'fill-parent 'wrap-content 1 'centre 5)
         ;;         (lambda ()
         ;;           (list
         ;;            (alert-dialog
         ;;             "vptest-lock"
         ;;             "Enter flight mode: phone needs hard reset to stop the program - are you sure?"
         ;;             (lambda (v)
         ;;               (cond
         ;;                ((eqv? v 1)
         ;;                 (list
         ;;                  (update-widget 'camera-preview (get-id "camerap") 'shutdown 0)
         ;;                  (start-activity "lock" 0 "")))
         ;;                (else
         ;;                 (list))))))))

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
         (json/gen-string (list 0 ""))))))

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
    (horiz

     (button (make-id "exit-button1")
             "1" 100 (layout 'wrap-content 'wrap-content -1 'left 5)
             (lambda ()
               (set-current! 'lock-unlock 0)
               (msg (get-current 'lock-unlock 0))
               '()))

     (space (layout 'fill-parent 'fill-parent 1 'left 5))

     (button (make-id "exit-button2")
             "2" 100 (layout 'wrap-content 'wrap-content -1 'right 5)
             (lambda ()
               (if (eqv? (get-current 'lock-unlock 0) 0)
                   (set-current! 'lock-unlock 1)
                   (set-current! 'lock-unlock 0))
               (msg (get-current 'lock-unlock 0))
               '())))

    (mtitle 'lock-title)
    (text-view (make-id "lock-program") "" 30 (layout 'fill-parent 'wrap-content -1 'centre 5) 'centre)
    (mtext 'lock-explanation)

    (camera-preview (make-id "lock-camerap") (layout 'fill-parent 320 1 'left 0))

    (space (layout 'fill-parent 'fill-parent 1 'left 5))

    (horiz
     (button (make-id "exit-button3")
             "3" 100 (layout 'wrap-content 'wrap-content -1 'left 5)
             (lambda ()
               (if (eqv? (get-current 'lock-unlock 0) 1)
                   (set-current! 'lock-unlock 2)
                   (set-current! 'lock-unlock 0))
               (msg (get-current 'lock-unlock 0))
               '()))

     (space (layout 'fill-parent 'fill-parent 1 'left 5))

     (button (make-id "exit-button4")
             "4" 100 (layout 'wrap-content 'wrap-content -1 'right 5)
             (lambda ()
               (msg (get-current 'lock-unlock 0))
               (cond
                ((eqv? (get-current 'lock-unlock 0) 2)
                 (set-current! 'lock-unlock 0)
                 (append
                  (clear-triggers)
                  (list
                   ;; shut it all down
                   (update-widget 'camera-preview (get-id "lock-camerap") 'shutdown 0)
                   (finish-activity 1))))
                (else
                 (set-current! 'lock-unlock 0)
                 '())))))


    )
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (entity-init! db "code" "program" (get-entity-by-unique db "code" arg))
     (set-current! 'camera-preview-id (get-id "lock-camerap"))

     ;; run the program
     (clear-triggers) ;; just in case

     (set! global-altitude (get-setting-value "alt"))
     (set! global-coverage (get-setting-value "coverage"))
     (set! global-timer (get-setting-value "timer"))

     (eval-library)

     (append
      (eval-program (entity-get-value "text"))
      (list
       (update-widget 'text-view (get-id "lock-program") 'text
                      (string-append "Running: " (entity-get-value "name")))
       (sensors-start
        "start-sensors"
        (get-current 'sensors '())
        (lambda (data)
          (set-current! 'sensor-values
                        (addv
                         (get-current 'sensor-values '())
                         (list (list-ref data 1)
                               (cdr (cdr (cdr (cdr data)))))))
          '()))
       )))

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
      (text-view 0 "Katrina Threadgill: research use case expert" 20 (layout 'fill-parent 'wrap-content -1 'centre 0))
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
