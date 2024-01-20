PRAGMA foreign_keys=OFF;
BEGIN TRANSACTION;
CREATE TABLE config (key TEXT PRIMARY KEY, val);
INSERT INTO config VALUES('json-prefix','test-');
INSERT INTO config VALUES('server-port',9090);
CREATE TABLE subject (id INTEGER PRIMARY KEY, name TEXT NOT NULL);
INSERT INTO subject VALUES(1,'foo');
INSERT INTO subject VALUES(2,'bar');
INSERT INTO subject VALUES(3,'meow');
CREATE TABLE object (id INTEGER PRIMARY KEY, name TEXT NOT NULL);
INSERT INTO object VALUES(1,'01');
INSERT INTO object VALUES(2,'03');
INSERT INTO object VALUES(3,'02');
INSERT INTO object VALUES(4,'04');
CREATE TABLE pref (id INTEGER PRIMARY KEY,
                              sub_id REFERENCES subject(id)
                                 ON UPDATE CASCADE ON DELETE CASCADE,
                              obj_id REFERENCES object(id)
                                 ON UPDATE CASCADE ON DELETE CASCADE,
                              val INTEGER NOT NULL DEFAULT 0);
INSERT INTO pref VALUES(3,1,4,2);
INSERT INTO pref VALUES(4,1,1,4);
INSERT INTO pref VALUES(5,2,1,0);
CREATE UNIQUE INDEX sub_name ON subject(name);
CREATE UNIQUE INDEX obj_name ON object(name);
CREATE UNIQUE INDEX sub_obj ON pref(sub_id,obj_id);
COMMIT;
