; Copyright (c) 2024, Natacha Porté
;
; Permission to use, copy, modify, and distribute this software for any
; purpose with or without fee is hereby granted, provided that the above
; copyright notice and this permission notice appear in all copies.
;
; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

(import (chicken file)
        (chicken io)
        (chicken irregex)
        (chicken process-context)
        (chicken string)
        (chicken time)
        (chicken time posix)
        intarweb
        spiffy
        sql-de-lite
        srfi-1
        srfi-18
        uri-common)

(define replaying? #f)

(define (command-line-argument k default-value)
  (let ((args (command-line-arguments)))
    (if (>= (length args) k)
        (list-ref args (sub1 k))
        default-value)))

(define (rfc-3339-local seconds)
  (let ((time-str (time->string (seconds->local-time seconds) "%FT%T%z")))
    (assert (= 24 (string-length time-str)))
    (if (equal? "0000" (substring time-str 20))
        (string-append (substring time-str 0 19) "Z")
        (string-append (substring time-str 0 22)
                       ":"
                       (substring time-str 22)))))

;;;;;;;;;;;;;
;; Tracing

(define trace-port
  (let ((name (command-line-argument 2 #f)))
    (cond ((not name) #f)
          ((equal? name "-") (current-output-port))
          (else (open-output-file name #:text #:append)))))

(define (trace-comment text)
  (write-line (string-append "; " text) trace-port))

(define trace-prev-time 0)
(define (trace-time)
  (let ((sec (current-seconds)))
    (unless (= sec trace-prev-time)
      (trace-comment (rfc-3339-local sec))
      (set! trace-prev-time sec))))

(define (trace-call obj)
  (trace-time)
  (write obj trace-port)
  (newline trace-port)
  (flush-output trace-port))

(define (trace-result obj result)
  (trace-time)
  (write-string ";" 1 trace-port)
; (write obj trace-port)
  (write-string " -> " 4 trace-port)
  (write result trace-port)
  (newline trace-port)
  (flush-output trace-port))

(define-syntax define-half-traced
  (syntax-rules ()
    ((define-half-traced (name . args) . body)
      (define (name . args)
        (trace-call (list 'name . args))
        . body))))

(define-syntax define-traced
  (syntax-rules ()
    ((define-traced (name . args) . body)
      (define (name . args)
        (trace-call (list 'name . args))
        (let ((result (begin . body)))
          (trace-result (list 'name . args) result)
          result)))))

(unless trace-port
  (set! trace-call (lambda (x) #f))
  (set! trace-comment (lambda (x) #f))
  (set! trace-result (lambda (x y) #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Database Creation/Migration

(define db
  (open-database (command-line-argument 1 "pref-matrix.sqlite")))

(exec (sql db "PRAGMA foreign_keys = ON;"))

(define (db-version)
  (query fetch-value (sql db "PRAGMA user_version;")))

(when (= 0 (db-version))
  (for-each
    (lambda (s) (exec (sql db s)))
    (list "PRAGMA user_version = 1;"
          "PRAGMA journal_mode = wal;"
          "PRAGMA synchronous = normal;"
          "CREATE TABLE config (key TEXT PRIMARY KEY, val);"
          "CREATE TABLE subject (id INTEGER PRIMARY KEY, name TEXT NOT NULL);"
          "CREATE TABLE object (id INTEGER PRIMARY KEY, name TEXT NOT NULL);"
          "CREATE TABLE pref (id INTEGER PRIMARY KEY,
                              sub_id REFERENCES subject(id)
                                 ON UPDATE CASCADE ON DELETE CASCADE,
                              obj_id REFERENCES object(id)
                                 ON UPDATE CASCADE ON DELETE CASCADE,
                              val INTEGER NOT NULL DEFAULT 0);"
          "CREATE UNIQUE INDEX sub_name ON subject(name);"
          "CREATE UNIQUE INDEX obj_name ON object(name);"
          "CREATE UNIQUE INDEX sub_obj ON pref(sub_id,obj_id);")))

(assert (= 1 (db-version)))

;;;;;;;;;;;;;;;;;;;
;; Database Query

(define (get-config key default-value)
  (let ((result (query fetch-value
                       (sql db "SELECT val FROM config WHERE key=?;")
                       key)))
    (if result result default-value)))

(define (object-id name)
  (query fetch-value (sql db "SELECT id FROM object WHERE name=?;") name))

(define (object-list)
  (query (map-rows car) (sql db "SELECT name FROM object ORDER BY name;")))

(define (subject-id name)
  (query fetch-value (sql db "SELECT id FROM subject WHERE name=?;") name))

(define (subject-list)
  (query (map-rows car) (sql db "SELECT name FROM subject ORDER BY name;")))

(define (subject-pref name start limit)
  (query fetch-rows
         (sql db "SELECT object.name,val FROM pref
                    OUTER LEFT JOIN object ON object.id = obj_id
                    OUTER LEFT JOIN subject ON subject.id = sub_id
                  WHERE subject.name=?
                  ORDER BY object.name
                  LIMIT ?,?;")
         name start limit))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data File Generation

(define json-escape-map
  '(("\x00" . "\\u0000")
    ("\x01" . "\\u0001")
    ("\x02" . "\\u0002")
    ("\x03" . "\\u0003")
    ("\x04" . "\\u0004")
    ("\x05" . "\\u0005")
    ("\x06" . "\\u0006")
    ("\a"   . "\\u0007")
    ("\b"   . "\\b")
    ("\t"   . "\\t")
    ("\n"   . "\\n")
    ("\v"   . "\\u000b")
    ("\f"   . "\\f")
    ("\r"   . "\\r")
    ("\x0e" . "\\u000e")
    ("\x0f" . "\\u000f")
    ("\x10" . "\\u0010")
    ("\x11" . "\\u0011")
    ("\x12" . "\\u0012")
    ("\x13" . "\\u0013")
    ("\x14" . "\\u0014")
    ("\x15" . "\\u0015")
    ("\x16" . "\\u0016")
    ("\x17" . "\\u0017")
    ("\x18" . "\\u0018")
    ("\x19" . "\\u0019")
    ("\x1a" . "\\u001a")
    ("\x1b" . "\\u001b")
    ("\x1c" . "\\u001c")
    ("\x1d" . "\\u001d")
    ("\x1e" . "\\u001e")
    ("\x1f" . "\\u001f")
    ("\""   . "\\\"")
    ("\\"   . "\\\\")))

(define (json-escape raw-str)
  (string-translate* raw-str json-escape-map))

(define (subject-json name)
  (string-append
    "{"
    (string-intersperse
      (map (lambda (row)
             (string-append "\""
                            (json-escape (car row))
                            "\":"
                            (number->string (cadr row))))
           (subject-pref name 0 -1))
      ",")
    "}"))

(define (all-json)
  (string-append
    "[[\""
    (string-intersperse (map json-escape (object-list)) "\",\"")
    "\"],{"
    (string-intersperse
      (map
        (lambda (name)
          (string-append "\"" (json-escape name) "\":" (subject-json name)))
        (subject-list))
      ",")
    "}]"))

(define (generate-json)
  (with-output-to-file
    (string-append
      (get-config "json-prefix" "")
      "all.json")
    (lambda () (write-string (all-json)))))

;;;;;;;;;;;;;;;;;;;;;
;; Database Updates

(define-traced (new-object name)
  (if (or (zero? (string-length name))
          (query fetch-value
                 (sql db "SELECT id FROM object WHERE name=?;")
                 name))
      #f
      (begin
        (exec (sql db "INSERT INTO object(name) VALUES (?);") name)
        (let ((result (last-insert-rowid db)))
          (unless replaying? (generate-json))
          result))))

(define-traced (new-subject name)
  (if (or (zero? (string-length name))
          (query fetch-value
                 (sql db "SELECT id FROM subject WHERE name=?;")
                 name))
      #f
      (begin
        (exec (sql db "INSERT INTO subject(name) VALUES (?);") name)
        (let ((result (last-insert-rowid db)))
          (unless replaying? (generate-json))
          result))))

(define-half-traced (set-config key val)
  (exec (sql db "INSERT OR REPLACE INTO config(key,val) VALUES (?,?);")
        key
        val))

; (define-half-traced (set-pref subject-name object-name value)
;   (exec (sql db "INSERT OR REPLACE INTO pref(sub_id,obj_id,val) VALUES
;                  ((SELECT id FROM subject WHERE name=?),
;                   (SELECT id FROM object WHERE name=?),
;                   ?);")
;         subject-name object-name value))

(define-half-traced (set-subject-pref subject-name alist)
  (let ((sub-id (subject-id subject-name)))
    (for-each
      (lambda (pair)
        (exec (sql db "INSERT OR REPLACE INTO pref(sub_id,obj_id,val)
                       VALUES (?,(SELECT id FROM object WHERE name=?),?);")
              sub-id
              (if (string? (car pair))
                  (car pair)
                  (symbol->string (car pair)))
              (string->number (cdr pair))))
      alist))
  (unless replaying? (generate-json)))

;;;;;;;;;;;
;; Replay

(let ((replay-str (command-line-argument 3 #f)))
  (when replay-str
    (set! replaying? #t)
    (load replay-str)
    (generate-json)
    (set! replaying? #f)))

;;;;;;;;;;;;;;;;;;;
;; Database Mutex

(define db-mutex
  (make-mutex "sqlite-db"))

(define-syntax with-db
  (syntax-rules ()
    ((with-db . op)
      (dynamic-wind
        (lambda () (mutex-lock! db-mutex))
        (lambda () (with-transaction db (lambda () . op)))
        (lambda () (mutex-unlock! db-mutex))))))

;;;;;;;;;;;;
;; Web API

(define cmd-list '())
(define (cmd-sleep)
  (let ((duration (get-config "tarpit" #f)))
    (when duration (sleep duration))))
(define-syntax defcmd
  (syntax-rules ()
    ((defcmd name . body)
      (set! cmd-list (cons (cons (symbol->string 'name)
                                 (lambda () (cmd-sleep) . body))
                           cmd-list)))))

(defcmd new-object
  (let* ((data (read-urlencoded-request-data (current-request)))
         (name (alist-ref 'name data eq? #f)))
    (if name
        (let ((result (with-db (new-object name))))
          (if result
              (send-status 'ok)
              (send-status 'conflict "Name already exists")))
        (send-status 'bad-request "Missing parameter"))))

(defcmd new-subject
  (let* ((data (read-urlencoded-request-data (current-request)))
         (name (alist-ref 'name data eq? #f)))
    (if name
        (let ((result (with-db (new-subject name))))
          (if result
              (send-status 'ok)
              (send-status 'conflict "Name already exists")))
        (send-status 'bad-request "Missing parameter"))))

(defcmd set-pref
  (let* ((data (read-urlencoded-request-data (current-request))))
    (if (eq? (caar data) 'sub)
        (begin
          (with-db
            (set-subject-pref
              (cdar data)
              (map
                (lambda (pair) (cons (symbol->string (car pair)) (cdr pair)))
                (cdr data))))
          (send-status 'ok))
        (send-status 'bad-request "Malformed request"))))

;;;;;;;;;;;;;;;
;; Web Server

(define (web-process continue)
  (cond ((not (eq? (request-method (current-request)) 'POST))
          (send-status 'method-not-allowed "This is a POST handler"))
        ((not (request-has-message-body? (current-request)))
          (send-status 'bad-request "Needs a body to process"))
        (else
          ((alist-ref (last (uri-path (request-uri (current-request))))
                      cmd-list
                      equal?
                      continue)))))

(server-port (get-config "server-port" 8080))
(vhost-map `((".*" . ,web-process)))
(start-server)
