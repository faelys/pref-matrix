PRAGMA foreign_keys=OFF;
BEGIN TRANSACTION;
CREATE TABLE config (key TEXT PRIMARY KEY, val ANY);
INSERT INTO config VALUES('json-prefix','test-');
INSERT INTO config VALUES('server-port',9090);
INSERT INTO config VALUES('default_topic','default');
CREATE TABLE topic (id INTEGER PRIMARY KEY,
                               name TEXT NOT NULL,
                               closed INTEGER NOT NULL DEFAULT 0);
INSERT INTO topic VALUES(1,'default',0);
CREATE TABLE subject (id INTEGER PRIMARY KEY,
                                 topic_id NOT NULL REFERENCES topic(id)
                                    ON UPDATE CASCADE ON DELETE CASCADE,
                                 name TEXT NOT NULL,
                                 hidden INTEGER NOT NULL DEFAULT 0);
INSERT INTO subject VALUES(1,1,'foo',0);
INSERT INTO subject VALUES(2,1,'bar',0);
INSERT INTO subject VALUES(3,1,'meow',0);
CREATE TABLE object (id INTEGER PRIMARY KEY,
                                topic_id NOT NULL REFERENCES topic(id)
                                   ON UPDATE CASCADE ON DELETE CASCADE,
                                name TEXT NOT NULL,
                                hidden INTEGER NOT NULL DEFAULT 0);
INSERT INTO object VALUES(1,1,'01',0);
INSERT INTO object VALUES(2,1,'03',0);
INSERT INTO object VALUES(3,1,'02',0);
INSERT INTO object VALUES(4,1,'04',0);
CREATE TABLE pref (id INTEGER PRIMARY KEY,
                              sub_id NOT NULL REFERENCES subject(id)
                                 ON UPDATE CASCADE ON DELETE CASCADE,
                              obj_id NOT NULL REFERENCES object(id)
                                 ON UPDATE CASCADE ON DELETE CASCADE,
                              val INTEGER NOT NULL DEFAULT 0);
INSERT INTO pref VALUES(3,1,4,2);
INSERT INTO pref VALUES(4,1,1,4);
INSERT INTO pref VALUES(5,2,1,0);
CREATE UNIQUE INDEX topic_name ON topic(name);
CREATE UNIQUE INDEX sub_name ON subject(topic_id,name);
CREATE UNIQUE INDEX obj_name ON object(topic_id,name);
CREATE INDEX v_sub_name ON subject(topic_id,name) WHERE hidden=0;
CREATE INDEX v_obj_name ON object(topic_id,name) WHERE hidden=0;
CREATE UNIQUE INDEX sub_obj ON pref(sub_id,obj_id);
COMMIT;
