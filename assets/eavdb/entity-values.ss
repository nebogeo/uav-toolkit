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
 "ktv-list.ss")

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; putting data in

;; get the type from the attribute table with an entity/key
(define (get-attribute-type db table entity-type key)
  (let ((sql (string-append
              "select attribute_type from " table
              "_attribute where entity_type = ? and attribute_id = ?")))
    (select-first db sql entity-type key)))

;; search for a type and add it if it doesn't exist
(define (find/add-attribute-type db table entity-type key type)
  (let ((t (get-attribute-type db table entity-type key)))
    ;; add and return passed in type if not exist
    (cond
      ((null? t)
       (msg "adding new attribute for" entity-type " called " key " of type " type)
       (db-insert
        db (string-append "insert into " table "_attribute values (null, ?, ?, ?)")
        key entity-type type)
       type)
      (else
       (cond
         ((equal? type t) t)
         (else
          (msg "type has changed for" entity-type key "from" t "to" type "???")
          ;; wont work
          ;; what do we do?
          ;; some kind of coercion for existing data???
          type))))))

;; low level insert of a ktv
(define (insert-value db table entity-id ktv dirty)
  ;; use type to dispatch insert to correct value table
  (db-insert db (string-append "insert into " table "_value_" (ktv-type ktv)
                               " values (null, ?, ?, ?, ?, 0)")
             entity-id (ktv-key ktv) (ktv-value ktv) (if dirty 1 0)))

;; update the value given an entity type, a attribute type and it's key (= attriute_id)
;; creates the value if it doesn't already exist, updates it otherwise if it's different
(define (update-value db table entity-id ktv)
  (let ((s (select-first
            db (string-append
                "select value from " table "_value_" (ktv-type ktv) " where entity_id = ? and attribute_id = ?")
            entity-id (ktv-key ktv))))
    (if (null? s)
        (insert-value db table entity-id ktv #t)
        ;; only update if the are different
        (if (not (ktv-eq? ktv (list (ktv-key ktv) (ktv-type ktv) s)))
            (db-exec
             db (string-append "update " table "_value_" (ktv-type ktv)
                               " set value=?, dirty=1  where entity_id = ? and attribute_id = ?")
             (ktv-value ktv) entity-id (ktv-key ktv))
            '())))) ;;(msg "values for" (ktv-key ktv) "are the same (" (ktv-value ktv) "==" s ")")))))

;; don't make dirty or update version here
(define (update-value-from-sync db table entity-id ktv)
  (let ((s (select-first
            db (string-append
                "select value from " table "_value_" (ktv-type ktv) " where entity_id = ? and attribute_id = ?")
            entity-id (ktv-key ktv))))
    ;;(msg "update-value-from-sync" s)
    ;;(msg ktv)
    ;;(msg entity-id)
    (if (null? s)
        (insert-value db table entity-id ktv #t) ;; <- don't make dirty!?
        (db-exec
         db (string-append "update " table "_value_" (ktv-type ktv)
                           " set value=?, dirty=0 where entity_id = ? and attribute_id = ?")
         (ktv-value ktv) entity-id (ktv-key ktv)))))

;; get all the (current) attributes for an entity type
(define (get-attribute-ids/types db table entity-type)
  (let ((s (db-select
            db (string-append
                "select * from " table "_attribute where entity_type = ?")
                entity-type)))
    (if (null? s) '()
        (map
         (lambda (row)
           (list (vector-ref row 1)    ;; id
                 (vector-ref row 3)))  ;; type
         (cdr s)))))

;; get the value, dirty and version given an entity type, a attribute type and it's key (= attriute_id)
(define (get-value db table entity-id kt)
  (let ((s (db-select
            db (string-append "select value, dirty from " table "_value_" (ktv-type kt)
                              " where entity_id = ? and attribute_id = ?")
            entity-id (ktv-key kt))))
    (if (null? s) '()
        (list (vector-ref (cadr s) 0)
              (vector-ref (cadr s) 1)))))

(define (clean-value db table entity-id kt)
  (db-exec db (string-append "update " table "_value_" (ktv-type kt)
                             " set dirty=0  where entity_id = ? and attribute_id = ?")
           entity-id (ktv-key kt)))

(define (dirtify-value db table entity-id kt)
  (db-exec db (string-append "update " table "_value_" (ktv-type kt)
                             " set dirty=1  where entity_id = ? and attribute_id = ?")
           entity-id (ktv-key kt)))
