#!/usr/bin/python
#-*- coding: utf-8 -*-

import sqlite3
import types

class SQLite3DB(object):
    def __init__(self, path):
        self.path = path
        self.conn = sqlite3.connect(path)
        if self.conn == None:
            raise Exception("Invalid db: %s" % self.path)

    def tables(self):
        cursor = self.conn.cursor()
        tables = []
        for row in cursor.execute("select name from sqlite_master where type='table'"):
            tables.append(row[0].encode("utf-8"))

        return tables

    def get_table(self, name):
        if name not in self.tables():
            raise Exception ("table %s does not exist")

        return Table(name, self.conn)

    def __str__(self):
        table_list = ", ".join(self.tables())
        return "db     : %s \ntables : %s" % (self.path, table_list)

class Table(object):
    def __init__(self, name, conn):
        self.name = name
        self.conn = conn

    def get_schema(self):
        cursor = self.conn.cursor()
        cursor.execute("select sql from sqlite_master where name='%s'" % self.name)
        r = cursor.fetchone()
        cursor.close()
        return r

    def row_count(self):
        cursor = self.conn.cursor()
        cursor.execute("select count(*) from %s" % self.name)
        r = cursor.fetchone()
        result = 0
        if r != None:
            result = int(r[0])

        cursor.close()
        return result

    def execute(self, sql):
        records = []
        cursor = self.conn.cursor()
        for row in cursor.execute(sql):
            r = Row(row)
            #print r
            records.append(r)

        cursor.close()
        return records

    def __str__(self):
        return "table name: %s, %s record(s)" % (self.name, self.row_count())
        
class Row(object):
    def __init__(self, r):
        self.field_count = len(r)
        self.data = r

    def __getitem__(self, index):
        return self.data[index]

    def __str__(self):
        res = []

        for field in self.data:
            if isinstance(field, types.NoneType):
                res.append("NULL")
            elif isinstance(field, types.IntType):
                res.append(str(field))
            elif isinstance(field, types.LongType):
                res.append(str(field))
            elif isinstance(field, types.StringType):
                res.append(field)
            elif isinstance(field, types.UnicodeType):
                res.append(field.encode("utf-8", 'ignore'))
            else:
                res.append("XXX")
        
        return "(%s)" % (",".join(res))


if __name__ == "__main__":

    db = SQLite3DB("History")
    #print db

    tables = [t for t in db.tables()]
    #print tables

    bm = db.get_table("urls")
    print bm
    print bm.get_schema()
    print bm.row_count()
    records = bm.execute("select * from urls")
    
    for r in records:
        print r

"""
    # record[2] == 
    titles = [ r[2] for r in records] # (id, url, title, visit_count .... favicon_id)
    # print urls
"""
