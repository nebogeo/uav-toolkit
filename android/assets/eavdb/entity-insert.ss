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
 "entity-get.ss")

(provide (all-defined-out))

;; insert an entire entity
(define (insert-entity db table entity-type user ktvlist)
  (insert-entity-wholesale db table entity-type (get-unique user) 1 0 ktvlist))

;; insert an entire entity
(define (insert-entity-with-id db table id entity-type user ktvlist)
  (insert-entity-wholesale-with-id db table id entity-type (get-unique user) 1 0 ktvlist))

;; insert an entire entity
(define (insert-entity/get-unique db table entity-type user ktvlist)
  (let ((uid (get-unique user)))
    (insert-entity-wholesale db table entity-type uid 1 0 ktvlist)
    uid))

;; used for the app preferences
(define (insert-entity-if-not-exists db table entity-type user entity-id ktvlist)
  (let ((found (get-entity-type db table entity-id)))
    (if (null? found)
        (insert-entity-with-id db table entity-id entity-type user ktvlist)
        #f)))

(define entity-sema (make-semaphore 1))

;; all the parameters - for syncing purposes
(define (insert-entity-wholesale db table entity-type unique-id dirty version ktvlist)
  (semaphore-wait entity-sema)
  (db-exec db "begin transaction")
  (let ((id (db-insert
             db (string-append
                 "insert into " table "_entity values (null, ?, ?, ?, ?)")
             entity-type unique-id dirty version)))

    ;; create the attributes if they are new, and validate them if they exist
    (for-each
     (lambda (ktv)
       (find/add-attribute-type db table entity-type (ktv-key ktv) (ktv-type ktv)))
     ktvlist)
    ;; add all the keys
    (for-each
     (lambda (ktv)
       (insert-value db table id ktv (not (zero? dirty))))
     ktvlist)

    (db-exec db "end transaction")
    (semaphore-post entity-sema)

    id))

(define (insert-entity-wholesale-with-id db table id entity-type unique-id dirty version ktvlist)
  (semaphore-wait entity-sema)
  (db-exec db "begin transaction")
  (let ((id (db-insert
             db (string-append
                 "insert into " table "_entity values (?, ?, ?, ?, ?)")
             id entity-type unique-id dirty version)))

    ;; create the attributes if they are new, and validate them if they exist
    (for-each
     (lambda (ktv)
       (find/add-attribute-type db table entity-type (ktv-key ktv) (ktv-type ktv)))
     ktvlist)
    ;; add all the keys
    (for-each
     (lambda (ktv)
       (insert-value db table id ktv dirty))
     ktvlist)

    (db-exec db "end transaction")
    (semaphore-post entity-sema)

    id))
