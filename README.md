This is a web application to gather how _subjects_ feel about a variety of _objects_. It is primarily used as a custom replacement of [Doodle][1] or [FramaDate][2] with more preference levels and a transposed presentation (so that when there are a lot of time slots and a few people, one scrolls vertically instead of horizontally).

It consists of a [CHICKEN Scheme][3] HTTP server which handles POST requests and updates a JSON representation of the data, and some static files served by a regular webserver (e.g. nginx).

It depends on [Spiffy][4] webserver and [sql-de-lite][5] binding of [SQLite][6] for persistent storage.

[1]: https://doodle.com/en/ "Free online meeting scheduling tool  | Doodle"
[2]: https://framadate.org/abc/en/ "Framadate - Make your polls"
[3]: https://call-cc.org/ "CHICKEN Scheme"
[4]: https://wiki.call-cc.org/eggref/5/spiffy "Spiffy - The CHICKEN Scheme wiki>"
[5]: https://wiki.call-cc.org/eggref/5/sql-de-lite "sql-de-lite - The CHICKEN Scheme wiki"
[6]: https://sqlite.org/index.html "SQLite Home Page"
