#lang racket
(require db "string-operations.rkt")

(struct master (db))
(struct numberlist (master alist))

(define (initalize-master! home)
  (define db (sqlite3-connect #:database 'memory #:mode 'create))
  (define the-master (master db))
  (unless (table-exists? db "numberstring")
    (query-exec db
                (string-append "CREATE TABLE numberstring" "(numbers TEXT)"))
    (master-numberstring-insert! the-master "1 2 3 4 5") the-master))

(define (master-numberstring-insert! master astring)
  (query-exec (master-db master)
              "INSERT INTO numberstring (numbers) VALUES (?)" astring))

(define (cleanse-table master)
  (query-exec (master-db master)
              "DELETE FROM numberstring"))

(define (greatest-sort-string master)
          (string-great-sort 
                 (query-value (master-db master)
                "SELECT numbers FROM numberstring")))

(define (least-sort-string master)
  (string-least-sort
   (query-value (master-db master)
                "SELECT numbers FROM numberstring")))

(provide initalize-master! cleanse-table least-sort-string greatest-sort-string master-numberstring-insert!)
