# encoding: utf-8

from struct import *

def bootstraping():
	f = open("database2.bin", "w+")
	s = "a s d f g h j q w e"
	for i in range(10):
		f.write(s)

# TODO
# 3. fill each record with string (update string)
# 4. convert code to functions, classes

# DONE
# 1. making basic record
# 2. making 10 basic record

def MakingBasicRecord(f, recNo, nextRec, string):
	f.write(pack(">I", recNo))
	f.write(pack(">I", nextRec))
	f.write(pack(">I", len(string)))

	length = 0x60 - 12
	for i in xrange(length):
		f.write("\0")

def SetUpTable(recCount):
	for i in range(recCount):
		recNo = i
		nextRecNo = i + 1
		if i == recCount-1:
			nextRecNo = i

		MakingBasicRecord(f, recNo, nextRecNo, "\0")

f = open("database2.bin", "w+")

SetUpTable(5)
