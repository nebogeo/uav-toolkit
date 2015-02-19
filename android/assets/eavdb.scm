;; MongooseWeb Copyright (C) 2013 Dave Griffiths
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

;; android/racket stuff
(define db-select db-exec)

;; racket
;(define db-exec exec/ignore)
;(define db-select select)
;(define db-insert insert)
;(define (db-status) "")
;(define (time) (list 0 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; entity-attribut-value system for sqlite
;;

;; create eav tables (add types as required)
(define (setup db table)
  (db-exec db (string-append "create table " table "_entity ( entity_id integer primary key autoincrement, entity_type varchar(256), unique_id varchar(256), dirty integer, version integer)"))
  (db-exec db (string-append "create table " table "_attribute ( id integer primary key autoincrement, attribute_id varchar(256), entity_type varchar(256), attribute_type varchar(256))"))
  (db-exec db (string-append "create table " table "_value_varchar ( id integer primary key autoincrement, entity_id integer, attribute_id varchar(255), value varchar(4096), dirty integer, version integer)"))
  (db-exec db (string-append "create table " table "_value_int ( id integer primary key autoincrement, entity_id integer, attribute_id varchar(255), value integer, dirty integer, version integer)"))
  (db-exec db (string-append "create table " table "_value_real ( id integer primary key autoincrement, entity_id integer, attribute_id varchar(255), value real, dirty integer, version integer)"))
  (db-exec db (string-append "create table " table "_value_file ( id integer primary key autoincrement, entity_id integer, attribute_id varchar(255), value varchar(4096), dirty integer, version integer)")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; basic key/type/value structure
;; used for all data internally, and maps to the eavdb types

(define (ktv key type value) (list key type value 0))
(define (ktv-with-version key type value version) (list key type value version))
(define (ktv-create key type value) (list key type value 0))
(define ktv-key car)
(define ktv-type cadr)
(define ktv-value caddr)
(define (ktv-version ktv) (list-ref ktv 3))

(define (ktv-eq? a b)
  (and
   (equal? (ktv-key a) (ktv-key b))
   (equal? (ktv-type a) (ktv-type b))
   (cond
    ((or
      (equal? (ktv-type a) "int")
      (equal? (ktv-type a) "real"))
     (eqv? (ktv-value a) (ktv-value b)))
    ((or
      (equal? (ktv-type a) "varchar")
      (equal? (ktv-type a) "file"))
     (equal? (ktv-value a) (ktv-value b)))
    (else
     (msg "unsupported ktv type in ktv-eq?: " (ktv-type a))
     #f))))

;; replace or insert a ktv
(define (ktvlist-replace ktv ktvlist)
  (cond
   ((null? ktvlist)
    (list ktv))
   ((equal? (ktv-key (car ktvlist)) (ktv-key ktv))
    (cons ktv (cdr ktvlist)))
   (else (cons (car ktvlist) (ktvlist-replace ktv (cdr ktvlist))))))

(define (ktvlist-merge a b)
  (foldl
   (lambda (ktv r)
     (ktvlist-replace ktv r))
   a b))

;; stringify based on type (for url)
(define (stringify-value ktv)
  (cond
   ((null? (ktv-value ktv)) "NULL")
   ((equal? (ktv-type ktv) "varchar") (string-append "'" (ktv-value ktv) "'"))
   (else
    (if (not (string? (ktv-value ktv)))
        (number->string (ktv-value ktv))
        (ktv-value ktv)))))

;; stringify based on type (for url)
(define (stringify-value-url ktv)
  (cond
   ((null? (ktv-value ktv)) "NULL")
   ((equal? (ktv-type ktv) "varchar") (ktv-value ktv))
   (else
    (if (not (string? (ktv-value ktv)))
        (number->string (ktv-value ktv))
        (ktv-value ktv)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; helper to return first instance from a select
(define (select-first db str . args)
  (let ((s (apply db-select (append (list db str) args))))
    (if (or (null? s) (eq? s #t))
        '()
        (vector-ref (cadr s) 0))))

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
                               " values (null, ?, ?, ?, ?, ?)")
             entity-id (ktv-key ktv) (ktv-value ktv) (if dirty 1 0) (ktv-version ktv)))

(define (get-unique user)
  (let ((t (time-of-day)))
    (string-append
     user "-" (number->string (car t)) ":" (number->string (cadr t)))))

;; insert an entire entity
(define (insert-entity db table entity-type user ktvlist)
  (insert-entity-wholesale db table entity-type (get-unique user) 1 0 ktvlist))

;; insert an entire entity
(define (insert-entity/get-unique db table entity-type user ktvlist)
  (let ((uid (get-unique user)))
    (insert-entity-wholesale db table entity-type uid 1 0 ktvlist)
    uid))

;; all the parameters - for syncing purposes
(define (insert-entity-wholesale db table entity-type unique-id dirty version ktvlist)
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
       (insert-value db table id ktv dirty))
     ktvlist)
    id))


;; update the value given an entity type, a attribute type and it's key (= attriute_id)
;; creates the value if it doesn't already exist, updates it otherwise if it's different
(define (update-value db table entity-id ktv)
  (let ((s (select-first
            db (string-append
				"select value from " table "_value_" (ktv-type ktv) " where entity_id = ? and attribute_id = ?")
			entity-id (ktv-key ktv))))
    (if (null? s)
        (insert-value db table entity-id ktv #t)
        ;; only update if they are different
        (if (not (ktv-eq? ktv (list (ktv-key ktv) (ktv-type ktv) s)))
            (begin
              (db-exec
               db (string-append "update " table "_value_" (ktv-type ktv)
                                 " set value=?, dirty=1, version=version+1  where entity_id = ? and attribute_id = ?")
               (ktv-value ktv) entity-id (ktv-key ktv)))
            '())))) ;;(msg "values for" (ktv-key ktv) "are the same (" (ktv-value ktv) "==" s ")")))))

;; don't make dirty or update version here
(define (update-value-from-sync db table entity-id ktv)
  ;;(msg "update-value-from-sync")
  ;;(msg entity-id ktv)
  (let ((s (select-first
            db (string-append
                "select value from " table "_value_" (ktv-type ktv) " where entity_id = ? and attribute_id = ?")
            entity-id (ktv-key ktv))))
    (if (null? s)
        (insert-value db table entity-id ktv #t)
        (begin
          ;;(msg "actually updating (fs)" (ktv-key ktv) "to" (ktv-value ktv))
          (db-exec
           db (string-append "update " table "_value_" (ktv-type ktv)
                             " set value=?, dirty=0, version=? where entity_id = ? and attribute_id = ?")
           (ktv-value ktv) (ktv-version ktv) entity-id (ktv-key ktv))))))

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
            db (string-append "select value, dirty, version from " table "_value_" (ktv-type kt)
                              " where entity_id = ? and attribute_id = ?")
            entity-id (ktv-key kt))))
    (if (null? s) '()
		(list (vector-ref (cadr s) 0)
			  (vector-ref (cadr s) 1)
			  (vector-ref (cadr s) 2)))))

;; get an entire entity, as a list of key/value pairs
(define (get-entity-plain db table entity-id)
  (let* ((entity-type (get-entity-type db table entity-id)))
    (cond
      ((null? entity-type) (msg "entity" entity-id "not found!") '())
      (else
       (foldl
        (lambda (kt r)
		  (let ((vdv (get-value db table entity-id kt)))
			(if (null? vdv)
				(begin
                  ;;(msg "ERROR: get-entity-plain: no value found for " entity-id " " (ktv-key kt))
                  r)
				(cons (list (ktv-key kt) (ktv-type kt)
                            (list-ref vdv 0) (list-ref vdv 2)) r))))
        '()
        (get-attribute-ids/types db table entity-type))))))

;; get an entire entity, as a list of key/value pairs, only dirty values
(define (get-entity-plain-for-sync db table entity-id)
  (let* ((entity-type (get-entity-type db table entity-id)))
    (cond
      ((null? entity-type) (msg "entity" entity-id "not found!") '())
      (else
       (foldl
        (lambda (kt r)
          (let ((vdv (get-value db table entity-id kt)))
            (cond
			 ((null? vdv)
			  ;;(msg "ERROR: get-entity-plain-for-sync: no value found for " entity-id " " (ktv-key kt))
			  r)
			 ;; only return if dirty
			 ((not (zero? (cadr vdv)))
			  (cons
			   (list (ktv-key kt) (ktv-type kt) (list-ref vdv 0) (list-ref vdv 2))
			   r))
			 (else r))))
        '()
        (get-attribute-ids/types db table entity-type))))))

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
        (let ((vdv (get-value db table entity-id kt)))
          (if (null? vdv)
              (begin
                ;;(msg "ERROR: get-entity-plain: no value found for " entity-id " " (ktv-key kt))
                r)
              (cons (list (ktv-key kt) (ktv-type kt)
                          (list-ref vdv 0) (list-ref vdv 2)) r))))
      '()
      kt-list))))


(define (all-entities db table type)
  (let ((s (db-select
            db (string-append "select e.entity_id from " table "_entity as e "
                              "join " table "_value_varchar "
                              " as n on n.entity_id = e.entity_id and n.attribute_id = ?"
                              "left join " table "_value_int "
                              "as d on d.entity_id = e.entity_id and d.attribute_id = ? "
                              "where e.entity_type = ? "
                              "and (d.value='NULL' or d.value is NULL or d.value = 0) "
                              "order by n.value")
            "name" "deleted" type)))
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

(define (build-query table filter)
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
     "as n on n.entity_id = e.entity_id and n.attribute_id = 'name' "
     ;; ignore deleted
     "join " table "_value_int "
     "as d on d.entity_id = e.entity_id and d.attribute_id = 'deleted' and "
     "d.value = 0 ")
    filter)
   "where e.entity_type = ? order by n.value"))

(define (build-args filter)
  (map
   (lambda (i)
     (filter-arg i))
   filter))

(define (filter-entities db table type filter)
  (let ((s (apply
            db-select
            (dbg (append
                  (list db (build-query table filter))
                  (build-args filter)
                  (list type))))))
    (msg (db-status db))
    (if (null? s)
        '()
        (map
         (lambda (i)
           (vector-ref i 0))
         (cdr s)))))


(define (validate db)
  ;; check attribute for duplicate entity-id/attribute-ids
  0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helpers

(define (ktv-get ktv-list key)
  (cond
   ((null? ktv-list) #f)
   ((equal? (ktv-key (car ktv-list)) key)
    (ktv-value (car ktv-list)))
   (else (ktv-get (cdr ktv-list) key))))

(define (ktv-get-type ktv-list key)
  (cond
   ((null? ktv-list) #f)
   ((equal? (ktv-key (car ktv-list)) key)
    (ktv-type (car ktv-list)))
   (else (ktv-get-type (cdr ktv-list) key))))

(define (ktv-set ktv-list ktv)
  (cond
   ((null? ktv-list) (list ktv))
   ((equal? (ktv-key (car ktv-list)) (ktv-key ktv))
    (cons ktv (cdr ktv-list)))
   (else (cons (car ktv-list) (ktv-set (cdr ktv-list) ktv)))))

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

;; only return name and photo
(define (db-filter-only db table type filter kt-list)
  (map
   (lambda (i)
     (get-entity-only db table i kt-list))
   (filter-entities db table type filter)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; updating data

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

(define (clean-value db table entity-id kt)
  (db-exec db (string-append "update " table "_value_" (ktv-type kt)
                             " set dirty=0  where entity_id = ? and attribute_id = ?")
           entity-id (ktv-key kt)))

(define (clean-entity-values db table entity-id)
  (let* ((entity-type (get-entity-type db table entity-id)))
    (cond
     ((null? entity-type)
      (msg "clean-entity-values: entity" entity-id "not found!") '())
     (else
      (for-each
       (lambda (kt)
         (clean-value db table entity-id (list (ktv-key kt) (ktv-type kt))))
       (get-attribute-ids/types db table entity-type))))))

;; update an entity, via a (possibly partial) list of key/value pairs
;; if dirty is not true, this is coming from a sync
(define (update-entity-values db table entity-id ktvlist dirty)
  ;;(msg "update-entity-values")
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
         ;;(msg ktv)
         (when (not (equal? (ktv-key ktv) "unique_id"))
			   (if dirty
				   (update-value db table entity-id ktv)
				   (update-value-from-sync db table entity-id ktv))))
       ktvlist)))))

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

(define (insert-entity-if-not-exists db table entity-type user entity-id ktvlist)
  (let ((found (get-entity-type db table entity-id)))
    (if (null? found)
        (insert-entity db table entity-type user ktvlist)
        #f)))

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

(define (update-entity-changed db table entity-id)
  (db-exec
   db (string-append
       "update " table "_entity set dirty=?, version=version+1 where entity_id = ?")
   1 entity-id))

;; set from a sync, so clear dirty - should be anyway
(define (update-entity-version db table entity-id version)
  (db-exec
   db (string-append
       "update " table "_entity set dirty=0, version=? where entity_id = ?")
   version entity-id))

(define (update-entity-clean db table unique-id)
  ;;(msg "cleaning")
  ;; clean entity table
  (db-exec
   db (string-append "update " table "_entity set dirty=? where unique_id = ?")
   0 unique-id)
  ;; clean value tables for this entity
  ;;(msg "cleaning values")
  (clean-entity-values db table (entity-id-from-unique db table unique-id))  )

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
    ;;(msg de)
    (if (null? de)
        '()
        (map
         (lambda (i)
           ;;(msg "dirty:" (vector-ref i 2))
           (list
            ;; build according to url ([table] entity-type unique-id dirty version)
            (cdr (vector->list i))
            (get-entity-plain-for-sync db table (vector-ref i 0))))
         (cdr de)))))

;; todo: BROKEN...
;; used for sync-all
(define (dirty-and-all-entities db table)
  (let ((de (db-select
             db (string-append
                 "select entity_id, entity_type, unique_id, dirty, version from " table "_entity"))))
    (if (null? de)
        '()
        (map
         (lambda (i)
           (list
            ;; build according to url ([table] entity-type unique-id dirty version)
            (cdr (vector->list i))
            ;; data entries (todo - only dirty values!)???????????
            (get-entity-plain db table (vector-ref i 0))))
         (cdr de)))))


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

(define (csv-titles db table entity-type)
  (foldl
   (lambda (kt r)
     (if (equal? r "") (string-append "\"" (ktv-key kt) "\"")
         (string-append r ", \"" (ktv-key kt) "\"")))
   "id, "
   (get-attribute-ids/types db table entity-type)))

(define (csv db table entity-type)
  (foldl
   (lambda (res r)
     (let ((entity (get-entity db table (vector-ref res 0))))
       (string-append
        r "\n"
        (foldl
         (lambda (ktv r)
           (cond
            ((equal? (ktv-key ktv) "unique_id") r)
            ((null? (ktv-value ktv))
             (msg "value not found in csv for " (ktv-key ktv))
             r)
            ;; dereferences lists of ids
            ((and
              (> (string-length (ktv-key ktv)) 8)
              (equal? (substring (ktv-key ktv) 0 8) "id-list-"))
             (string-append r ", \"" (get-entity-names db "sync" (string-split (ktv-value ktv) '(#\,))) "\""))
            ;; look for unique ids and dereference them
            ((and
              (> (string-length (ktv-key ktv)) 3)
              (equal? (substring (ktv-key ktv) 0 3) "id-"))
             (string-append r ", \"" (get-entity-name db "sync" (ktv-value ktv)) "\""))
            (else
             (string-append r ", \"" (stringify-value-url ktv) "\""))))
         (vector-ref res 1) ;; unique_id
         entity))))
   (csv-titles db table entity-type)
   (cdr (db-select
         db (string-append
             "select entity_id, unique_id from "
             table "_entity where entity_type = ?") entity-type))))
