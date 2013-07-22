CREATE TABLE Account (
  _id integer primary key autoincrement, 
  CONFLICT integer default 0, 
  conflict integer default 1, 
  cbaCertificateAlias text );

CREATE TABLE password (
  _id INTEGER PRIMARY KEY, 
  host TEXT, 
  username TEXT, 
  password TEXT, 
  UNIQUE (host, username) ON CONFLICT REPLACE);
