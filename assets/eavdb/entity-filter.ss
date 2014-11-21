#lang racket

;; Starwisp Copyright (C) 2014 Dave Griffiths
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

(require
 "../web/scripts/utils.ss"
 "../web/scripts/sql.ss"
 "ktv.ss"
 "ktv-list.ss"
 "entity-values.ss"
 "entity-insert.ss"
 "entity-get.ss"
 "entity-update.ss")

(provide (all-defined-out))

;; filter is list of (attribute-key type op arg) e.g. ("gender" "varchar" "=" "Female")
;; note: only one filter per key..

(define (make-filter k t o a) (list k t o a))
(define (filter-key f) (list-ref f 0))
(define (filter-type f) (list-ref f 1))
(define (filter-op f) (list-ref f 2))
(define (filter-arg f) (list-ref f 3))

(define (merge-filter f fl)
  (cond
   ((null? fl) (list f))
   ((equal? (filter-key (car fl)) (filter-key f))
    (cons f (cdr fl)))
   (else (cons (car fl) (merge-filter f (cdr fl))))))

(define (delete-filter key fl)
  (cond
   ((null? fl) '())
   ((equal? (filter-key (car fl)) key)
    (cdr fl))
   (else (cons (car fl) (delete-filter key (cdr fl))))))

;; replace - with _
(define (mangle var)
  (list->string
   (map
    (lambda (c)
      (cond
       ((eqv? c #\-) #\_)
       (else c)))
    (string->list var))))

(define (build-query table filter typed)
  (string-append
   (foldl
    (lambda (i r)
      (let ((var (mangle (string-append (filter-key i) "_var"))))
        ;; add a query chunk
        (string-append
         r "join " table "_value_" (filter-type i) " "
         "as " var " on "
         var ".entity_id = e.entity_id and " var ".attribute_id = '" (filter-key i) "' and "
         var ".value " (filter-op i) " ? ")))

    ;; boilerplate query start
    (string-append
     "select e.entity_id from " table "_entity as e "
     ;; order by name
     "join " table "_value_varchar "
     "as n on n.entity_id = e.entity_id and n.attribute_id = 'name' "
     ;; ignore deleted
     "join " table "_value_int "
     "as d on d.entity_id = e.entity_id and d.attribute_id = 'deleted' and "
     "d.value = 0 ")
    filter)
   (if typed
       (if (equal? typed "mongoose")
           (begin
             (msg "ordering chop...")
             "where e.entity_type = ? order by substr(n.value,3)")
           "where e.entity_type = ? order by n.value")
       "order by n.value")))

(define (build-query-inc-deleted table filter)
  (string-append
   (foldl
    (lambda (i r)
      (let ((var (string-append (filter-key i) "_var")))
        ;; add a query chunk
        (string-append
         r "join " table "_value_" (filter-type i) " "
         "as " var " on "
         var ".entity_id = e.entity_id and " var ".attribute_id = '" (filter-key i) "' and "
         var ".value " (filter-op i) " ? ")))

    ;; boilerplate query start
    (string-append
     "select e.entity_id from " table "_entity as e "
     ;; order by name
     "join " table "_value_varchar "
     "as n on n.entity_id = e.entity_id and n.attribute_id = 'name' ")
    filter)
   "where e.entity_type = ? order by value"))


(define (build-args filter)
  (map
   (lambda (i)
     (filter-arg i))
   filter))

(define (filter-entities db table type filter)
  (let ((q (build-query table filter (not (equal? type "*")))))
    (let ((s (apply
	      db-select
	      (append
	       (list db q)
	       (build-args filter)
	       (list type)))))
      (msg (db-status db))
      (if (null? s)
	  '()
	  (map
	   (lambda (i)
	     (vector-ref i 0))
	   (cdr s))))))

(define (filter-entities-inc-deleted db table type filter)
  (let ((q (build-query-inc-deleted table filter)))
  (let ((s (apply
            db-select
            (append
             (list db q)
             (build-args filter)
             (if (equal? type "*") '() (list type))))))
    (msg (db-status db))
    (if (null? s)
        '()
        (map
         (lambda (i)
           (vector-ref i 0))
         (cdr s))))))
