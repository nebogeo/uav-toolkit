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
 "entity-values.ss")

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; getting data out

(define (entity-exists? db table unique-id)
  (not (null? (select-first
               db (string-append
                   "select * from " table "_entity where unique_id = ?")
               unique-id))))

(define (get-entity-type db table entity-id)
  (select-first
   db (string-append
       "select entity_type from " table "_entity where entity_id = ?")
       entity-id))

(define (get-all-entity-types db table)
  (cdr (db-select db (string-append "select distinct entity_type from " table "_entity;"))))

;; fold over values - fn takes ktv, dirty and accum
(define (fold-entity fn db table entity-id)
  (let* ((entity-type (get-entity-type db table entity-id)))
    (cond
      ((null? entity-type) (msg "entity" entity-id "not found!") '())
      (else
       (foldl
        (lambda (kt r)
          (let ((vd (get-value db table entity-id kt)))
            (fn kt vd r)))
        '()
        (reverse (get-attribute-ids/types db table entity-type)))))))


;; get an entire entity, as a list of key/value pairs
(define (get-entity-plain db table entity-id)
  (fold-entity
   (lambda (kt vd r)
     (if (null? vd)
         r (cons (ktv (ktv-key kt) (ktv-type kt) (car vd)) r)))
   db table entity-id))

;; get an entire entity, as a list of key/value pairs, only dirty values
(define (get-entity-plain-for-sync db table entity-id)
  (fold-entity
   (lambda (kt vd r)
     (cond
      ((null? vd) r)
      ;; only return if dirty
      ((not (zero? (cadr vd)))
       (cons
        (list (ktv-key kt) (ktv-type kt) (list-ref vd 0)) r))
      (else r)))
   db table entity-id))

;; get an entire entity, as a list of key/value pairs maintaining order by filling
;; out null values - only use for csv building
(define (get-entity-for-csv db table entity-id)
  (fold-entity
   (lambda (kt vd r)
     (if (null? vd)
         (cons (list (ktv-key kt) (ktv-type kt) (null-value-for-type (ktv-type kt))) r)
         (cons (ktv (ktv-key kt) (ktv-type kt) (car vd)) r)))
   db table entity-id))

;; get an entire entity, as a list of key/value pairs (includes entity id)
(define (get-entity db table entity-id)
  (let ((unique-id (get-unique-id db table entity-id)))
    (cons
     (list "unique_id" "varchar" unique-id)
     (get-entity-plain db table entity-id))))

;; like get-entity-plain, but only look for specific key/types - for speed
(define (get-entity-only db table entity-id kt-list)
  (let ((unique-id (get-unique-id db table entity-id)))
    (cons
     (list "unique_id" "varchar" unique-id)
     (foldl
      (lambda (kt r)
        (let ((vd (get-value db table entity-id kt)))
          (if (null? vd)
              (begin
                ;;(msg "ERROR: get-entity-plain: no value found for " entity-id " " (ktv-key kt))
                r)
              (cons (ktv (ktv-key kt) (ktv-type kt) (car vd)) r))))
      '()
      kt-list))))


(define (all-entities db table type)
  (msg "all-entities" type)
  (let ((s (db-select
            db (dbg (string-append "select e.entity_id from " table "_entity as e "
                              "join " table "_value_varchar "
                              " as n on n.entity_id = e.entity_id and n.attribute_id = ? "
                              "left join " table "_value_int "
                              "as d on d.entity_id = e.entity_id and d.attribute_id = ? "
                              "where e.entity_type = ? "
                              "and (d.value='NULL' or d.value is NULL or d.value = 0) "
                              "order by n.value"))
            "name" "deleted" type)))
    (msg s)
    (msg (db-status db))
    (if (null? s)
        '()
        (map
         (lambda (i)
           (vector-ref i 0))
         (cdr s)))))

(define (all-entities-with-parent db table type parent)
  (let ((s (db-select
            db (string-append "select e.entity_id from " table "_entity as e "
                              "join " table "_value_varchar "
                              " as n on n.entity_id = e.entity_id and n.attribute_id = ?"
                              "join " table "_value_varchar "
                              " as p on p.entity_id = e.entity_id and p.attribute_id = ?"
                              "left join " table "_value_int "
                              "as d on d.entity_id = e.entity_id and d.attribute_id = ? "
                              "where e.entity_type = ? and "
                              "p.value = ? and "
                              "(d.value='NULL' or d.value is NULL or d.value = 0) "
                              "order by n.value")
            "name" "parent" "deleted" type parent)))
    (msg (db-status db))
    (if (null? s)
        '()
        (map
         (lambda (i)
           (vector-ref i 0))
         (cdr s)))))

(define (all-unique-ids db table)
  (let ((s (db-select
            db (string-append "select e.unique_id from " table "_entity as e "))))
    (if (null? s)
        '()
        (map
         (lambda (i)
           (vector-ref i 0))
         (cdr s)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; doing things with unique ids

(define (entity-id-from-unique db table unique-id)
  (select-first
   db (string-append "select entity_id from " table "_entity where unique_id = ?")
   unique-id))

(define (entity-version-from-unique db table unique-id)
  (select-first
   db (string-append "select version from " table "_entity where unique_id = ?")
   unique-id))


(define (get-unique-id db table entity-id)
  (select-first
   db (string-append
       "select unique_id from " table "_entity where entity_id = ?")
       entity-id))

(define (get-entity-id db table unique-id)
  (select-first
   db (string-append
       "select entity_id from " table "_entity where unique_id = ?")
   unique-id))

(define (get-entity-by-unique db table unique-id)
  (get-entity db table (get-entity-id db table unique-id)))

(define (get-entity-name db table unique-id)
  (ktv-get (get-entity-by-unique db table unique-id) "name"))

(define (get-entity-names db table id-list)
  (foldl
   (lambda (id r)
     (if (equal? r "")
         (get-entity-name db table id)
         (string-append r ", " (get-entity-name db table id))))
   ""
   id-list))
