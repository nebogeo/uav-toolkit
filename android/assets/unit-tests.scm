(asserteq "filter" (filter (lambda (i) (odd? i)) (list 0 1 2 3)) (list 1 3))
(asserteq "sort" (sort (list 3 2 0 1) <) (list 0 1 2 3))
(asserteq "find" (find 3 (list '(3 30) '(2 20) '(0 100) '(1 10))) (list 3 30))
(asserteq "build-list" (build-list (lambda (i) (* i 2)) 5) (list 0 2 4 6 8))
(asserteq "foldl" (foldl (lambda (i r) (+ i r)) 0 (list 1 2 3 4)) 10)
(asserteq "insert-to" (insert-to 999 3 (list 0 1 2 3 4)) (list 0 1 2 999 3 4))
(asserteq "list-replace" (list-replace (list 1 2 3 4) 2 100) (list 1 2 100 4))
(asserteq "insert" (insert 4 < (list 2 5 100)) (list 2 4 5 100))

(assert "date<" (date< (list 20 12 2010) (list 25 12 2010)))
(asserteq "date->string" (date->string (list 20 12 2012)) "20/12/2012")

(asserteq "scheme->json" (scheme->json (list 10)) "[10]")
(asserteq "scheme->json2" (scheme->json (list 10 20)) "[10, 20]")
(asserteq "scheme->json3" (scheme->json (list (list "one" "two") 10))
          "[[\"one\", \"two\"], 10]")
(asserteq "scheme->json4" (scheme->json (list)) "[]")
(asserteq "scheme->json5" (scheme->json 'sym) "\"sym\"")
(asserteq "scheme->json6" (scheme->json (list #t #f)) "[true, false]")
(asserteq "assoc->json" (assoc->json '((one . 1) (two . "three")))
          "{\n\"one\": 1,\n\"two\": \"three\"\n}")


;; db
(msg "testing db")
(define db "unit-test.db")
(db-open db)

(define (feq a b)
  (< (abs (- a b)) 0.001))

;;(msg (db-status db))

;; test low level sql
(db-exec db "create table unittest ( id integer primary key autoincrement, name varchar(256), num int, r real )")

(define id (db-insert db "insert into unittest values (null, ?, ?, ?)" "hello" 23 1.1))
(asserteq "sql autoinc" (+ id 1) (db-insert db "insert into unittest values (null, ?, ?, ?)" "hello2" 26 2.3))

(let ((q (db-exec db "select * from unittest")))
  (assert "sql length" (> (length q) 2)))

(let ((q (db-exec db "select * from unittest where id = ?" id)))
  (asserteq "sql select one" (length q) 2)
  (assert "sql select two" (vector? (car q)))
  (asserteq "sql select 3" (vector-ref (cadr q) 2) 23)
  (assert "sql select 4" (feq (vector-ref (cadr q) 3) 1.1)))

(db-exec db "update unittest set name=? where id = ?" "bob" id)

(let ((q (db-exec db "select * from unittest where id = ?" id)))
  (asserteq "sql update" (vector-ref (cadr q) 1) "bob"))

(db-exec db "update unittest set name=? where id = ?" "Robert'); DROP TABLE unittest;--" id)

(let ((q (db-exec db "select * from unittest where id = ?" id)))
  (asserteq "bobby tables sql injection" (vector-ref (cadr q) 1) "Robert'); DROP TABLE unittest;--"))


;; test the entity attribute value system
(define table "eavunittest")
(setup db table)

(asserteq "ktv one" (stringify-value (ktv "one" "varchar" "two")) "'two'")
(asserteq "ktv 2" (stringify-value (ktv "one" "int" 3)) "3")
(asserteq "ktv 3" (stringify-value-url (ktv "one" "varchar" "two")) "two")
(asserteq "ktv 4" (stringify-value-url (ktv "one" "int" 3)) "3")

(asserteq "select first" (select-first db "select name from unittest where id = ?" (+ id 1))
          "hello2")

(define e (insert-entity db table "thing" "me" (list (ktv "param1" "varchar" "bob")
                                                     (ktv "param2" "int" 30)
                                                     (ktv "param3" "real" 3.141))))

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

(update-entity db table e (list (ktv "param1" "varchar" "wotzit")
                                (ktv "param2" "int" 1)))

(let ((e (get-entity db table e)))
  (asserteq "update-entity 1" (ktv-get e "param1") "wotzit")
  (asserteq "update-entity 2" (ktv-get e "param2") 1))

(update-entity db table e (list (ktv "param3" "real" 3.3)))

(let ((e (get-entity db table e)))
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

;; test the versioning
(asserteq "dirty flag" (get-entity-dirty db table e2) 1)
(let ((uid (get-unique-id db table e2)))
  (update-entity-clean db table uid))
(asserteq "dirty flag post clean" (get-entity-dirty db table e2) 0)
(asserteq "versioning" (get-entity-version db table e) 2)
(assert "dirty" (> (length (dirty-entities db table)) 0))

(for-each
 (lambda (e)
   (update-entity-clean
    db table
    (list-ref (car e) 1)))
 (dirty-entities db table))

(asserteq "cleaning" (length (dirty-entities db table)) 0)

(msg (db-status db))

(msg "testing some interface building...")

(setup db "sync")

(define i (insert-entity
           db "sync" "pack" "user"
           (list (ktv "name" "varchar" "pack one"))))

(define p (get-entity db "sync" i))

(msg (ktv-get p "unique_id"))

(define (make-mongoose name)
  (insert-entity
   db "sync" "mongoose" (ktv-get p "unique_id") 
   (list
    (ktv "name" "varchar" name)
    (ktv "gender" "varchar" "Female")
    (ktv "litter-code" "varchar" "34")
    (ktv "chip-code" "varchar" "34")
    (ktv "pack-id" "varchar" "unique_id")
    )))

(make-mongoose "bob")
(make-mongoose "fred")
(make-mongoose "arnold")
(make-mongoose "lucy")
(make-mongoose "doris")
(make-mongoose "kylie")
(make-mongoose "jenny")


(for-each 
 (lambda (fragment)
   (msg "calling fragment" fragment)
   (fragment-callback 'on-create fragment '("")))
 (build-list 
  (lambda (i) 
    (choose (list
             "pf-timer"
             "pf-scan1"
             "events"
             "pf-timer"
             "ev-pupfeed"
             "ev-pupcare"
             "ev-pupfind"
             "ev-pupaggr"
             "ev-grpint"
             "ev-grpalarm"
             "ev-grpmov")))
  100))


