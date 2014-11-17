;; Naked on Pluto Copyright (C) 2010 Aymeric Mansoux, Marloes de Valk, Dave Griffiths
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

#lang scheme

(require
 "../web/scripts/utils.ss"
 "../web/scripts/sql.ss"
 "ktv.ss"
 "ktv-list.ss"
 "entity-values.ss"
 "entity-get.ss")

(provide (all-defined-out))

(define (csv-titles db table entity-type)
  (foldl
   (lambda (kt r)
     (if (equal? r "") (string-append "\"" (ktv-key kt) "\"")
         (string-append r ", \"" (ktv-key kt) "\"")))
   "id "
   (get-attribute-ids/types db table entity-type)))

(define (csv-old db table entity-type)
  (let ((s (db-select
         db (string-append
             "select entity_id, unique_id from "
             table "_entity where entity_type = ?") entity-type)))
    (msg "CSV ------------------------------>" entity-type)
    (msg s)
    (if (null? s)
	;; nothing here, just return titles
	(csv-titles db table entity-type)
	(foldl
	 (lambda (res r)
	   (msg res)
	   (let ((entity (get-entity-for-csv db table (vector-ref res 0))))
	     (string-append
	      r "\n"
	      (foldl
	       (lambda (ktv r)
		 (msg ktv)
		 (cond
		  ((equal? (ktv-key ktv) "unique_id") r)
		  ((null? (ktv-value ktv))
		   (msg "value not found in csv for " (ktv-key ktv))
		   (string-append r ", NULL"))
		  ;; dereferences lists of ids
		  ((and
		    (> (string-length (ktv-key ktv)) 8)
		    (equal? (substring (ktv-key ktv) 0 8) "id-list-"))
		   (let ((ids (string-split (ktv-value ktv) '(#\,))))
		     (if (null? ids)
			 (string-append r ", \"\"")
			 (string-append r ", \"" (get-entity-names db "sync"  "\"")))))
		  ;; look for unique ids and dereference them
		  ((and
		    (> (string-length (ktv-key ktv)) 3)
		    (equal? (substring (ktv-key ktv) 0 3) "id-")
		    (not (equal? (ktv-value ktv) "none")))
		   (msg "looking up name")
		   (msg ktv)
		   (let ((name (get-entity-name db "sync" (ktv-value ktv))))
		     (if (null? name)
			 "\"nobody\""
			 (string-append r ", \"" name "\""))))
		  (else
		   (string-append r ", \"" (stringify-value-url ktv) "\""))))
	       (vector-ref res 1) ;; unique_id
	       entity))))
	 (csv-titles db table entity-type)
	 (cdr s)))))

(define (csv db table entity-type)
  (let ((s (db-select
         db (string-append
             "select entity_id, unique_id from "
             table "_entity where entity_type = ?") entity-type)))
    (msg "CSV ------------------------------>" entity-type)
    (if (null? s)
	;; nothing here, just return titles
	(csv-titles db table entity-type)
	(foldl
	 (lambda (res r)
	   (let ((entity (get-entity-for-csv db table (vector-ref res 0))))
	     (string-append
	      r "\n"
	      (foldl
	       (lambda (ktv r)
		 (cond
		  ((equal? (ktv-key ktv) "unique_id") r)
		  ((null? (ktv-value ktv))
		   (msg "value not found in csv for " (ktv-key ktv))
		   (string-append r ", NULL"))
		  ;; dereferences lists of ids
		  (else
		   (string-append r ", \"" (stringify-value-url ktv) "\""))))
	       (vector-ref res 1) ;; unique_id
	       entity))))
	 (csv-titles db table entity-type)
	 (cdr s)))))

;; exporting human editable reports

(define (deref-entity db entity)
  (foldl
   (lambda (ktv r)
     (append
      r
      (list
       (ktv-key ktv)
       (cond
        ;; dereferences lists of ids
        ((and
          (> (string-length (ktv-key ktv)) 8)
          (equal? (substring (ktv-key ktv) 0 8) "id-list-"))
         (get-entity-names db "sync" (string-split (ktv-value ktv) '(#\,))))
        ;; look for unique ids and dereference them
        ((and
          (> (string-length (ktv-key ktv)) 3)
          (equal? (substring (ktv-key ktv) 0 3) "id-"))
         (get-entity-name db "sync" (ktv-value ktv)))
        (else
         (ktv-value ktv))))))
   '()
   entity))


(define (csv-convert col)
  (if (number? col) (number->string col)
      (if (string? col) col
          (begin
            (msg "csvify found:" col) "oops"))))

;; convert list of lists into comma seperated columns
;; and newline seperated rows
(define (csvify l)
  (foldl
   (lambda (row r)
     (let ((row-text
            (foldl
             (lambda (col r)
               (let ((converted (csv-convert col)))
                 (if (equal? r "")
                     converted
                     (string-append r ", " converted))))
             "" row)))
       (msg row-text)
       (string-append r row-text "\n")))
   "" l))


(define (ktv-filter ktv-list key)
  (filter
   (lambda (ktv)
     (not (equal? (ktv-key ktv) key)))
   ktv-list))

(define (ktv-filter-many ktv-list key-list)
  (foldl
   (lambda (key r)
     (ktv-filter r key))
   ktv-list
   key-list))

;; meant to be general, but made for pup focal reports
;(define (export-csv db table parent-entity entity-types)
;  (let* ((focal (get-entity db "sync" (get-entity-id db "sync" (ktv-get parent-entity "id-focal-subject"))))
;         (pack (get-entity db "sync" (get-entity-id db "sync" (ktv-get focal "pack-id")))))
;    (csvify
;     (cons
;      '("time" "user" "pack" "subject" "observation type" "key" "value" "key" "value")
;      (sort
;       (foldl
;        (lambda (entity-type r)
;          (append
;           r (map
;              (lambda (entity)
;                (append
;                 (list
;                  (ktv-get entity "time")
;                  (ktv-get entity "user")
;                 (ktv-get pack "name")
;                 (ktv-get focal "name")
;                 entity-type)
;                 (deref-entity 
;		  db (ktv-filter-many
;		      entity (list "user" "unique_id" "parent" "time")))))
;              (db-all-with-parent
;               db table entity-type
;               (ktv-get parent-entity "unique_id")))))
;        '()
;        entity-types)
;      (lambda (a b)
;        (string<? (car a) (car b))))))))

