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
