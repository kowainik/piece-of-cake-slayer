-- To execute this file from SQL REPL:
-- \i sql/schema.sql

-----------------
-- BASE TABLES --
-----------------

CREATE TABLE IF NOT EXISTS items
( id         SERIAL      PRIMARY KEY
, text       TEXT        NOT NULL
);
