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

(define-activity-list
  (activity
   "main"
   (linear-layout
    (make-id "top")
    'vertical
    (layout 'fill-parent 'fill-parent 1 'left)
    (list
     (spinner (make-id "spinner") (list "one" "two" "three" "cows") fillwrap
              (lambda (v)
                (display "spinner fn called")(newline)
                (list (update-widget 'text-view (get-id "view3") 'text v))))
     (edit-text (make-id "name") "Name" 20 fillwrap
                (lambda (v) (list (update-widget 'text-view 999 'text v))))
     (linear-layout
      (make-id "foo")
      'horizontal
      (layout 'fill-parent 'fill-parent 1 'centre)
      (list
       (button (make-id "but1") "Click me" 20 (layout 'wrap-content 'wrap-content 0 'centre)
               (lambda () (list (update-widget 'text-view 999 'hide 0))))
       (button (make-id "but3") "Boo" 20 (layout 'wrap-content 'wrap-content 0 'centre)
               (lambda () (list (update-widget 'text-view 999 'hide 0))))))

     (text-view (make-id "view1") "This is the title" 10 fillwrap)
     (text-view (make-id "view2") "More texht" 40 fillwrap)
     (text-view (make-id "view3") "event More texht" 30 fillwrap)

     (button (make-id "but2") "Click me also pretty please" 20 fillwrap
             (lambda ()
               (list
                (toast "hello dudes")
                (start-activity "two" 2)
                (update-widget 'text-view (get-id "view1") 'text "I have been updated"))))
     (seek-bar (make-id "seek") 100 fillwrap
               (lambda (v)
                 (list
                  (update-widget 'text-view (get-id "view2") 'text (number->string v))
                  (update-widget 'canvas (get-id "canvas") 'drawlist
                                 (list (drawlist-line '(255 0 0) 10 (list 0 0 v 100))))
                  )))

     (canvas (make-id "canvas")
             (layout 200 200 1 'centre)
             (list
              (drawlist-line '(255 0 0) 5 '(0 0 100 100))))

     (button (make-id "but4") "one two" 10 fillwrap
             (lambda ()
               '()))))

   (lambda (activity)
     (activity-layout activity))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '()))

  (activity
   "two"
   (linear-layout
    (make-id "top")
    'vertical
    (layout 'fill-parent 'fill-parent 1 'left)
    (list
     (spinner (make-id "spinner") (list "one" "two" "three" "cows") fillwrap
              (lambda (v)
                (list (toast "what's up doc?"))))
     (image-view (make-id "face") "face" wrap)
     (button (make-id "exit") "Exit" 50 fillwrap
             (lambda ()
               (list (finish-activity 99))))))

   (lambda (activity)
     (activity-layout activity))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())))
