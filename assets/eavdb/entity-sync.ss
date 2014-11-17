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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; versioning

(define (get-entity-version db table entity-id)
  (select-first
   db (string-append "select version from " table "_entity where entity_id = ?")
   entity-id))

(define (get-entity-dirty db table entity-id)
  (select-first
   db (string-append "select dirty from " table "_entity where entity_id = ?")
   entity-id))

(define (update-entity-clean db table unique-id)
  (db-exec
   db (string-append "update " table "_entity set dirty=? where unique_id = ?")
   0 unique-id)
  (clean-entity-values db table (entity-id-from-unique db table unique-id))  )

;; for when remote entities don't exist for whatever reason
(define (update-entity-dirtify db table unique-id)
  (db-exec
   db (string-append "update " table "_entity set dirty=? where unique_id = ?")
   1 unique-id)
  (dirtify-entity-values db table (entity-id-from-unique db table unique-id))  )

(define (have-dirty? db table)
  (not (zero?
        (select-first
         db (string-append "select count(entity_id) from " table "_entity where dirty=1")))))

(define (get-dirty-stats db table)
  (list
   (select-first
    db (string-append "select count(entity_id) from " table "_entity where dirty=1"))
   (select-first
    db (string-append "select count(entity_id) from " table "_entity;"))))

(define (dirty-entities db table)
  (let ((de (db-select
             db (string-append
                 "select entity_id, entity_type, unique_id, dirty, version from "
                 table "_entity where dirty=1 limit 5;"))))
    (if (null? de)
        '()
        (map
         (lambda (i)
           ;;(msg "dirty-entities")
           (list
            ;; build according to url ([table] entity-type unique-id dirty version)
            (cdr (vector->list i))
            (get-entity-plain-for-sync db table (vector-ref i 0))))
         (cdr de)))))

;; include all the ktvs
(define (dirty-entities-for-review db table)
  (let ((de (db-select
             db (string-append
                 "select entity_id, entity_type, unique_id, dirty, version from " table "_entity where dirty=1;"))))
    (if (null? de)
        '()
        (map
         (lambda (i)
           ;;(msg "dirty-entities")
           (list
            ;; build according to url ([table] entity-type unique-id dirty version)
            (cdr (vector->list i))
            (get-entity-plain db table (vector-ref i 0))))
         (cdr de)))))


;; todo: BROKEN...
;; used for sync-all
;(define (dirty-and-all-entities db table)
;  (let ((de (db-select
;             db (string-append
;                 "select entity_id, entity_type, unique_id, dirty, version from " table "_entity"))))
;    (if (null? de)
;        '()
;        (map
;         (lambda (i)
;           (list
;            ;; build according to url ([table] entity-type unique-id dirty version)
;            (cdr (vector->list i))
;            ;; data entries (todo - only dirty values!)???????????
;            (get-entity-plain db table (vector-ref i 0))))
;         (cdr de)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; syncing

(define (stringify-list l)
  (foldl
   (lambda (i r)
     (string-append r " " i))
   "" l))

(define (stringify-ktvlist ktvlist)
  (foldl
   (lambda (i r)
     (string-append r " " (ktv-key i) ":" (stringify-value i)))
   ""
   ktvlist))

(define (build-sync-debug db table)
  (foldl
   (lambda (i r)
     (string-append
      r "\n" (vector-ref i 0) " " (vector-ref i 1) " "
      (stringify-ktvlist (get-entity db table (vector-ref i 0)))))
   ""
   (cdr (db-select
         db (string-append "select * from " table "_entity where dirty=1;")))))


(define (build-sync db table)
  (map
   (lambda (i)
     (list
      (vector->list i)
      (get-entity db table (vector-ref i 0))))
   (cdr (db-select
         db (string-append "select * from " table "_entity where dirty=1;")))))


(define (entity-sync-test db table)

  (define e (insert-entity db table "thing" "me" (list (ktv "param1" "varchar" "bob")
                                                       (ktv "param2" "int" 30)
                                                       (ktv "param3" "real" 3.141)
                                                       (ktv "name" "varchar" "name")
                                                       (ktv "deleted" "int" 0))))

  (define e2 (insert-entity db table "thing" "me"
                            (list (ktv "param1" "varchar" "bob")
                                  (ktv "param2" "int" 30)
                                  (ktv "param3" "real" 3.141)
                                  (ktv "param4" "int" 0))))

  (update-entity db table e (list (ktv "param1" "varchar" "wotzit")
                                  (ktv "param2" "int" 1)))
  (update-entity db table e (list (ktv "param3" "real" 3.3)))


  ;; test the versioning
  (asserteq "dirty flag" (get-entity-dirty db table e) 1)
  (asserteq "dirty flag2" (get-entity-dirty db table e2) 1)
  (let ((uid (get-unique-id db table e2)))
    (update-entity-clean db table uid))
  (asserteq "dirty flag post clean" (get-entity-dirty db table e2) 0)
  (asserteq "versioning" (get-entity-version db table e) 2)
  (asserteq "dirty flag3" (get-entity-dirty db table e) 1)
  (assert "dirty" (> (length (dbg (dirty-entities db table))) 0))

  (for-each
   (lambda (e)
     (update-entity-clean
      db table
      (list-ref (car e) 1)))
   (dirty-entities db table))

  (asserteq "cleaning" (length (dirty-entities db table)) 0)

  )
