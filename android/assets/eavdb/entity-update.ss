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
 "entity-get.ss"
 "entity-insert.ss")

(provide (all-defined-out))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; updating data

(define (update-entity-changed db table entity-id)
  (db-exec
   db (string-append
       "update " table "_entity set dirty=?, version=version+1 where entity_id = ?")
   1 entity-id))

(define (update-entity-version db table entity-id version)
  (db-exec
   db (string-append
       "update " table "_entity set dirty=0, version=? where entity_id = ?")
   version entity-id))

;; update an entire entity (version incl), via a (possibly partial) list of key/value pairs
(define (update-to-version db table entity-id version ktvlist)
  ;; not dirty
  (update-entity-values db table entity-id ktvlist #f)
  (update-entity-version db table entity-id version))

;; auto update version
(define (update-entity db table entity-id ktvlist)
  ;; dirty
  (update-entity-changed db table entity-id)
  (update-entity-values db table entity-id ktvlist #t))

(define (clean-entity-values db table entity-id)
  ;;(msg "clean-entity-values")
  (let* ((entity-type (get-entity-type db table entity-id)))
    (cond
     ((null? entity-type)
      (msg "clean-entity-values: entity" entity-id "not found!") '())
     (else
      (for-each
       (lambda (kt)
         (clean-value db table entity-id (list (ktv-key kt) (ktv-type kt))))
       (get-attribute-ids/types db table entity-type))))))

(define (dirtify-entity-values db table entity-id)
  ;;(msg "clean-entity-values")
  (semaphore-wait entity-sema)
  (db-exec db "begin transaction")
  (let* ((entity-type (get-entity-type db table entity-id)))
    (cond
     ((null? entity-type)
      (msg "dirtify-entity-values: entity" entity-id "not found!") '())
     (else
      (for-each
       (lambda (kt)
         (dirtify-value db table entity-id (list (ktv-key kt) (ktv-type kt))))
       (get-attribute-ids/types db table entity-type)))))
  (db-exec db "end transaction")
  (semaphore-post entity-sema))

;; update an entity, via a (possibly partial) list of key/value pairs
;; if dirty is not true, this is coming from a sync
(define (update-entity-values db table entity-id ktvlist dirty)
  (semaphore-wait entity-sema)
  (db-exec db "begin transaction")
  (let* ((entity-type (get-entity-type db table entity-id)))
    (cond
     ((null? entity-type) (msg "entity" entity-id "not found!") '())
     (else
      ;; update main entity type
      (for-each
       (lambda (ktv)
         (when (not (equal? (ktv-key ktv) "unique_id"))
               (find/add-attribute-type db table entity-type (ktv-key ktv) (ktv-type ktv))))
       ktvlist)
      (for-each
       (lambda (ktv)
         (if dirty
             (update-value db table entity-id ktv)
             (update-value-from-sync db table entity-id ktv)))
       ktvlist))))
  (db-exec db "end transaction")
  (semaphore-post entity-sema))

;; update or create an entire entity if it doesn't exist
;; will return the new entity id if it's created
(define (update/insert-entity db table entity-type user entity-id ktvlist)
  (let* ((entity-type (get-entity-type db table entity-id)))
    (cond
     ((null? entity-type)
      (insert-entity db table entity-type user ktvlist))
     (else
      (update-entity db table entity-id ktvlist)
      #f))))

(define (entity-update-test db table)

  (define e (insert-entity db table "thing" "me" (list (ktv "param1" "varchar" "bob")
                                                       (ktv "param2" "int" 30)
                                                       (ktv "param3" "real" 3.141)
                                                       (ktv "name" "varchar" "name")
                                                       (ktv "deleted" "int" 0))))

  (asserteq "eav ent type" (get-entity-type db table e) "thing")

  (let ((e (get-entity db table e)))
    (asserteq "entity get 1" (ktv-get e "param1") "bob")
    (asserteq "entity get 2" (ktv-get e "param2") 30)
    (assert "entity get 3" (feq (ktv-get e "param3") 3.141)))

  (update-value db table e (ktv "param1" "varchar" "fred"))

  (let ((e (get-entity db table e)))
    (asserteq "update value 1" (ktv-get e "param1") "fred")
    (asserteq "update value 2" (ktv-get e "param2") 30))

  (assert "all-entities" (> (length (all-entities db table "thing")) 0))

  (msg "hello")

  (update-entity db table e (list (ktv "param1" "varchar" "wotzit")
                                  (ktv "param2" "int" 1)))

  (let ((e (get-entity db table e)))
    (asserteq "update-entity 1" (ktv-get e "param1") "wotzit")
    (asserteq "update-entity 2" (ktv-get e "param2") 1))

  (update-entity db table e (list (ktv "param3" "real" 3.3)))

  (let ((e (get-entity db table e)))
    (msg e)
    (asserteq "update-entity 3" (ktv-get e "param1") "wotzit")
    (asserteq "update-entity 4" (ktv-get e "param2") 1)
    (assert "update-entity 5" (feq (ktv-get e "param3") 3.3)))

  (define e2 (insert-entity db table "thing" "me"
                            (list (ktv "param1" "varchar" "bob")
                                  (ktv "param2" "int" 30)
                                  (ktv "param3" "real" 3.141)
                                  (ktv "param4" "int" 0))))

  (let ((e (get-entity db table e2)))
    (msg e)
    (asserteq "new entity 1" (ktv-get e "param1") "bob")
    (asserteq "new entity 2" (ktv-get e "param2") 30)
    (assert "new entity 3" (feq (ktv-get e "param3") 3.141))
    (asserteq "new entity 3" (ktv-get e "param4") 0))

  )
