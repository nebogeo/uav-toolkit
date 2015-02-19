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

;; common code - require and provide ignored on tinyscheme

(require (planet jaymccarthy/sqlite:5:1/sqlite))

(require
 "../web/scripts/utils.ss"
 "../web/scripts/sql.ss"
 "ktv.ss"
 "ktv-list.ss"
 "entity-values.ss"
 "entity-insert.ss"
 "entity-get.ss"
 "entity-update.ss"
 "entity-sync.ss"
 "entity-filter.ss")

(provide (all-defined-out))

(msg "hello from eavdb.ss")


(define (upgrade-table db name)
  (db-exec db (string-append "alter table " name " add version integer")))


;; create eav tables (add types as required)
(define (setup db table)
  (msg "db setup")
  (db-exec db (string-append "create table " table "_entity ( entity_id integer primary key autoincrement, entity_type varchar(256), unique_id varchar(256), dirty integer, version integer)"))
  (db-exec db (string-append "create table " table "_attribute ( id integer primary key autoincrement, attribute_id varchar(256), entity_type varchar(256), attribute_type varchar(256))"))
  (db-exec db (string-append "create table " table "_value_varchar ( id integer primary key autoincrement, entity_id integer, attribute_id varchar(255), value varchar(4096), dirty integer, version integer)"))
  (upgrade-table db (string-append table "_value_varchar"))
  (db-exec db (string-append "create table " table "_value_int ( id integer primary key autoincrement, entity_id integer, attribute_id varchar(255), value integer, dirty integer, version integer)"))
  (upgrade-table db (string-append table "_value_int"))
  (db-exec db (string-append "create table " table "_value_real ( id integer primary key autoincrement, entity_id integer, attribute_id varchar(255), value real, dirty integer, version integer)"))
  (upgrade-table db (string-append table "_value_real"))
  (db-exec db (string-append "create table " table "_value_file ( id integer primary key autoincrement, entity_id integer, attribute_id varchar(255), value varchar(4096), dirty integer, version integer)"))
  (upgrade-table db (string-append table "_value_file")))


(define (validate db)
  ;; check attribute for duplicate entity-id/attribute-ids
  0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helpers

(define (db-all db table type)
  (map
   (lambda (i)
     (get-entity db table i))
   (all-entities db table type)))

(define (db-with-parent db table type parent)
  (map
   (lambda (i)
     (get-entity db table i))
   (all-entities-with-parent db table type parent)))

(define (db-filter db table type filter)
  (map
   (lambda (i)
     (get-entity db table i))
   (filter-entities db table type filter)))

(define (db-filter-inc-deleted db table type filter)
  (map
   (lambda (i)
     (get-entity db table i))
   (filter-entities-inc-deleted db table type filter)))

;; only return (eg. name and photo)
(define (db-filter-only db table type filter kt-list)
  (map
   (lambda (i)
     (get-entity-only db table i kt-list))
   (filter-entities db table type filter)))

;; only return (eg. name and photo)
(define (db-filter-only-inc-deleted db table type filter kt-list)
  (map
   (lambda (i)
     (get-entity-only db table i kt-list))
   (filter-entities-inc-deleted db table type filter)))
