#!/usr/bin/env python
# encoding: utf-8

class Session:
    def Dispose(self):
        print "Disposed"

    def BeginTransaction(self):
        print "Begin Transaction"

    def EndTransaction(self):
        print "End Transaction"

class Multimedia:
    def AddRecord(self, session):
      pass

class Results:
    @staticmethod
    def GetSession():
        return Session()

    @staticmethod
    def Multimedia():
        return Multimedia()
    
class SessionWrapper:
    def __init__(self, rs):
        self.rs = rs
        self.rs.BeginTransaction()

    def session(self):
        return self.rs

    def __enter__(self):
        return self

    def __exit__(self, type, value, traceback):
        self.rs.EndTransaction()
        self.rs.Dispose()

"""
# 1
session = Results.GetSession()
mm = Results.Multimedia()
r  = mm.AddRecord(session)

# 2
try:
    session = Results.GetSession()
    mm = Results.Multimedia()
    r = mm.AddRecord(session)
finally:
    session.Dispose()
"""
# 3 
with SessionWrapper(Results.GetSession()) as sw:
    mm = Results.Multimedia()
    r = mm.AddRecord(sw.session())
