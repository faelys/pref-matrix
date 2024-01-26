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

(define (trace-call name args)
  (trace-time)
  (write-string "(" #f trace-port)
  (write name trace-port)
  (for-each
    (lambda (arg)
      (write-string
        (if (or (string? arg) (number? arg)) " " " '")
        #f trace-port)
      (write arg trace-port))
    args)
  (write-line ")" trace-port)
  (flush-output trace-port))

(define (trace-result name args result)
  (trace-time)
  (write-string "; -> " #f trace-port)
  (write result trace-port)
  (newline trace-port)
  (flush-output trace-port))

(define-syntax define-half-traced
  (syntax-rules ()
    ((define-half-traced (name . args) . body)
      (define (name . args)
        (trace-call 'name (list . args))
        . body))))

(define-syntax define-traced
  (syntax-rules ()
    ((define-traced (name . args) . body)
      (define (name . args)
        (trace-call 'name (list . args))
        (let ((result (begin . body)))
          (trace-result 'name (list . args) result)
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
    (lambda (s) (exec (sql/transient db s)))
    (list "PRAGMA user_version = 2;"
          "PRAGMA journal_mode = wal;"
          "PRAGMA synchronous = normal;"
          "CREATE TABLE config (key TEXT PRIMARY KEY, val ANY);"
          "CREATE TABLE topic (id INTEGER PRIMARY KEY,
                               name TEXT NOT NULL,
                               closed INTEGER NOT NULL DEFAULT 0);"
          "CREATE TABLE subject (id INTEGER PRIMARY KEY,
                                 topic_id NOT NULL REFERENCES topic(id)
                                    ON UPDATE CASCADE ON DELETE CASCADE,
                                 name TEXT NOT NULL,
                                 hidden INTEGER NOT NULL DEFAULT 0);"
          "CREATE TABLE object (id INTEGER PRIMARY KEY,
                                topic_id NOT NULL REFERENCES topic(id)
                                   ON UPDATE CASCADE ON DELETE CASCADE,
                                name TEXT NOT NULL,
                                hidden INTEGER NOT NULL DEFAULT 0);"
          "CREATE TABLE pref (id INTEGER PRIMARY KEY,
                              sub_id NOT NULL REFERENCES subject(id)
                                 ON UPDATE CASCADE ON DELETE CASCADE,
                              obj_id NOT NULL REFERENCES object(id)
                                 ON UPDATE CASCADE ON DELETE CASCADE,
                              val INTEGER NOT NULL DEFAULT 0);"
          "CREATE UNIQUE INDEX topic_name ON topic(name);"
          "CREATE UNIQUE INDEX sub_name ON subject(topic_id,name);"
          "CREATE UNIQUE INDEX obj_name ON object(topic_id,name);"
          "CREATE INDEX v_sub_name ON subject(topic_id,name) WHERE hidden=0;"
          "CREATE INDEX v_obj_name ON object(topic_id,name) WHERE hidden=0;"
          "CREATE UNIQUE INDEX sub_obj ON pref(sub_id,obj_id);")))

(when (= 1 (db-version))
  (with-transaction db (lambda ()
    (for-each
      (lambda (s) (exec (sql/transient db s)))
      (list "ALTER TABLE config RENAME TO old_config;"
            "ALTER TABLE subject RENAME TO old_subject;"
            "ALTER TABLE object RENAME TO old_object;"
            "ALTER TABLE pref RENAME TO old_pref;"
            "CREATE TABLE config (key TEXT PRIMARY KEY, val ANY);"
            "INSERT INTO config(key,val) SELECT key,val FROM old_config;"
            "INSERT OR IGNORE INTO config(key,val)
               VALUES ('default_topic','default');"
            "DROP TABLE old_config;"
            "CREATE TABLE topic (id INTEGER PRIMARY KEY,
                               name TEXT NOT NULL,
                               closed INTEGER NOT NULL DEFAULT 0);"
            "INSERT INTO topic(id,name)
               SELECT 1,val FROM config WHERE key='default_topic';"
            "CREATE TABLE subject (id INTEGER PRIMARY KEY,
                                 topic_id NOT NULL REFERENCES topic(id)
                                    ON UPDATE CASCADE ON DELETE CASCADE,
                                 name TEXT NOT NULL,
                                 hidden INTEGER NOT NULL DEFAULT 0);"
            "INSERT INTO subject(id,topic_id,name)
               SELECT id,1,name FROM old_subject;"
            "CREATE TABLE object (id INTEGER PRIMARY KEY,
                                topic_id NOT NULL REFERENCES topic(id)
                                   ON UPDATE CASCADE ON DELETE CASCADE,
                                name TEXT NOT NULL,
                                hidden INTEGER NOT NULL DEFAULT 0);"
            "INSERT INTO object(id,topic_id,name)
               SELECT id,1,name FROM old_object;"
            "CREATE TABLE pref (id INTEGER PRIMARY KEY,
                              sub_id NOT NULL REFERENCES subject(id)
                                 ON UPDATE CASCADE ON DELETE CASCADE,
                              obj_id NOT NULL REFERENCES object(id)
                                 ON UPDATE CASCADE ON DELETE CASCADE,
                              val INTEGER NOT NULL DEFAULT 0);"
            "INSERT INTO pref(id,sub_id,obj_id,val)
               SELECT id,sub_id,obj_id,val FROM old_pref;"
            "DROP TABLE old_pref;"
            "DROP TABLE old_subject;"
            "DROP TABLE old_object;"
            "CREATE UNIQUE INDEX topic_name ON topic(name);"
            "CREATE UNIQUE INDEX sub_name ON subject(topic_id,name);"
            "CREATE UNIQUE INDEX obj_name ON object(topic_id,name);"
            "CREATE INDEX v_sub_name ON subject(topic_id,name) WHERE hidden=0;"
            "CREATE INDEX v_obj_name ON object(topic_id,name) WHERE hidden=0;"
            "CREATE UNIQUE INDEX sub_obj ON pref(sub_id,obj_id);"
            "PRAGMA user_version = 2;")))))

(assert (= 2 (db-version)))

;;;;;;;;;;;;;;;;;;;
;; Database Query

(define (get-config key default-value)
  (let ((result (query fetch-value
                       (sql db "SELECT val FROM config WHERE key=?;")
                       key)))
    (if result result default-value)))

(define (topic-file-name tid)
  (query fetch-value (sql db "SELECT name FROM topic WHERE id=?;") tid))

(define (topic-id name)
  (query fetch-value (sql db "SELECT id FROM topic WHERE name=?;") name))

(define (writable-topic-id name)
  (let* ((resolved-name (if name name (get-config "default_topic" #f)))
         (row (if resolved-name
                  (query fetch-row
                         (sql db "SELECT id,closed FROM topic WHERE name=?;")
                         resolved-name)
                  '())))
    (if (and (not (null? row)) (zero? (cadr row)))
        (car row)
        #f)))

(define (topic-id-list)
  (query (map-rows car) (sql db "SELECT id FROM topic;")))

(define (object-id tid name)
  (query fetch-value
         (sql db "SELECT id FROM object WHERE topic_id=? AND name=?;")
         tid
         name))

(define (object-list tid)
  (query (map-rows car)
         (sql db "SELECT name FROM object
                  WHERE topic_id=? AND hidden=0 ORDER BY name;")
         tid))

(define (subject-id tid name)
  (query fetch-value
         (sql db "SELECT id FROM subject WHERE topic_id=? AND name=?;")
         tid
         name))

(define (subject-list tid)
  (query (map-rows car)
         (sql db "SELECT name FROM subject
                  WHERE topic_id=? AND hidden=0 ORDER BY name;")
         tid))

(define (subject-pref tid name)
  (query fetch-rows
         (sql db "SELECT object.name,val FROM pref
                    OUTER LEFT JOIN object ON object.id = obj_id
                    OUTER LEFT JOIN subject ON subject.id = sub_id
                  WHERE subject.topic_id=? AND subject.name=?
                    AND object.hidden=0
                  ORDER BY object.name")
         tid name))

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

(define valid-file-name-irregex
  (sre->irregex '(+ (or alphanumeric "-" "_"))))
(define (valid-file-name? name)
  (irregex-match? valid-file-name-irregex name))

(define (json-escape raw-str)
  (string-translate* raw-str json-escape-map))

(define (subject-json tid name)
  (string-append
    "{"
    (string-intersperse
      (map (lambda (row)
             (string-append "\""
                            (json-escape (car row))
                            "\":"
                            (number->string (cadr row))))
           (subject-pref tid name))
      ",")
    "}"))

(define (topic-json tid)
  (string-append
    "[[\""
    (string-intersperse (map json-escape (object-list tid)) "\",\"")
    "\"],{"
    (string-intersperse
      (map
        (lambda (name)
          (string-append "\""
                         (json-escape name)
                         "\":"
                         (subject-json tid name)))
        (subject-list tid))
      ",")
    "}]"))

(define (generate-topic-json tid)
  (let ((name (topic-file-name tid)))
    (when (valid-file-name? name)
      (with-output-to-file
        (string-append
          (get-config "json-prefix" "")
          name
          ".json")
        (lambda () (write-string (topic-json tid)))))))

(define (generate-json)
  (for-each generate-topic-json (topic-id-list)))

;;;;;;;;;;;;;;;;;;;;;
;; Database Updates

(define-traced (new-topic name)
  (if (or (zero? (string-length name)) (topic-id name))
      #f
      (begin
        (exec (sql db "INSERT INTO topic(name) VALUES (?);") name)
        (let ((result (last-insert-rowid db)))
          (unless replaying? (generate-topic-json result))
          result))))

(define-traced (new-object topic name)
  (let ((tid (writable-topic-id topic)))
    (if (or (not tid) (zero? (string-length name)))
        #f
        (let ((row (query fetch-row
                          (sql db "SELECT id,hidden FROM object
                                   WHERE topic_id=? AND name=?;")
                          tid
                          name)))
          (cond ((null? row)
                  (exec
                    (sql db "INSERT INTO object(topic_id,name) VALUES (?,?);")
                    tid name)
                  (let ((result (last-insert-rowid db)))
                    (unless replaying? (generate-topic-json tid))
                    result))
                ((zero? (cadr row)) #f)
                (else
                  (exec (sql db "UPDATE object SET hidden=0 WHERE id=?;")
                        (car row))
                  (unless replaying? (generate-topic-json tid))
                  (car row)))))))

(define-half-traced (hide-object topic name)
  (let* ((tid (writable-topic-id topic))
         (oid (if tid (object-id tid name) #f)))
    (when oid
      (exec (sql db "UPDATE object SET hidden=1 WHERE id=?;") oid)
      (unless replaying? (generate-topic-json tid)))))

(define-traced (new-subject topic name)
  (let ((tid (writable-topic-id topic)))
    (if (or (not tid) (zero? (string-length name)))
        #f
        (let ((row (query fetch-row
                          (sql db "SELECT id,hidden FROM subject
                                   WHERE topic_id=? AND name=?;")
                          tid
                          name)))
          (cond ((null? row)
                  (exec
                    (sql db "INSERT INTO subject(topic_id,name) VALUES (?,?);")
                    tid name)
                  (let ((result (last-insert-rowid db)))
                    (unless replaying? (generate-topic-json tid))
                    result))
                ((zero? (cadr row)) #f)
                (else
                  (exec (sql db "UPDATE subject SET hidden=0 WHERE id=?;")
                        (car row))
                  (unless replaying? (generate-topic-json tid))
                  (car row)))))))

(define-half-traced (hide-subject topic name)
  (let* ((tid (writable-topic-id topic))
         (sid (if tid (subject-id tid name) #f)))
    (when sid
      (exec (sql db "UPDATE subject SET hidden=1 WHERE id=?;") sid)
      (unless replaying? (generate-topic-json tid)))))

(define-half-traced (set-config key val)
  (exec (sql db "INSERT OR REPLACE INTO config(key,val) VALUES (?,?);")
        key
        val))

(define (set-pref tid sub-id object-name value)
  (let ((obj-id (object-id tid object-name)))
    (if obj-id
        (begin
          (exec (sql db "INSERT OR REPLACE INTO pref(sub_id,obj_id,val)
                         VALUES (?,?,?);")
                sub-id
                obj-id
                value)
          (last-insert-rowid db))
        #f)))

(define-traced (set-subject-pref topic-name subject-name alist)
  (let* ((tid (writable-topic-id topic-name))
         (sub-id (if tid (subject-id tid subject-name) #f)))
    (if sub-id
        (let ((result
          (map
            (lambda (pair)
              (set-pref tid sub-id (car pair) (string->number (cdr pair))))
            alist)))
          (unless replaying? (generate-topic-json tid))
          result)
        #f)))

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

(defcmd hide-object
  (let* ((data (read-urlencoded-request-data (current-request)))
         (topic (alist-ref 'topic data eq? #f))
         (name  (alist-ref 'name  data eq? #f)))
    (if name
        (begin
          (with-db (hide-object topic name))
          (send-status 'ok))
        (send-status 'bad-request "Missing parameter"))))

(defcmd hide-subject
  (let* ((data (read-urlencoded-request-data (current-request)))
         (topic (alist-ref 'topic data eq? #f))
         (name  (alist-ref 'name  data eq? #f)))
    (if name
        (begin
          (with-db (hide-subject topic name))
          (send-status 'ok))
        (send-status 'bad-request "Missing parameter"))))

(defcmd new-topic
  (let* ((data (read-urlencoded-request-data (current-request)))
         (name (alist-ref 'name data eq? #f)))
    (if name
        (let ((result (with-db (new-topic name))))
          (if result
              (send-status 'ok)
              (send-status 'conflict "Name already exists")))
        (send-status 'bad-request "Missing parameter"))))

(defcmd new-object
  (let* ((data (read-urlencoded-request-data (current-request)))
         (topic (alist-ref 'topic data eq? #f))
         (name  (alist-ref 'name  data eq? #f)))
    (if name
        (let ((result (with-db (new-object topic name))))
          (if result
              (send-status 'ok)
              (send-status 'conflict "Name already exists")))
        (send-status 'bad-request "Missing parameter"))))

(defcmd new-subject
  (let* ((data (read-urlencoded-request-data (current-request)))
         (topic (alist-ref 'topic data eq? #f))
         (name  (alist-ref 'name  data eq? #f)))
    (if name
        (let ((result (with-db (new-subject topic name))))
          (if result
              (send-status 'ok)
              (send-status 'conflict "Name already exists")))
        (send-status 'bad-request "Missing parameter"))))

(defcmd set-pref
  (let* ((all-data (read-urlencoded-request-data (current-request)))
         (topic (if (eq? (caar all-data) 'topic) (cdar all-data) #f))
         (data  (if topic (cdr all-data) all-data)))
    (if (eq? (caar data) 'sub)
        (begin
          (with-db
            (set-subject-pref
              topic
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
