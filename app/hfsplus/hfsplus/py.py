#!/usr/bin/env python

# -*- coding:utf-8 -*-
import plistlib
import pprint
import struct
import os

from keystore.keybag  import Keybag

pp = pprint.PrettyPrinter(indent=4)

def tlvToDict(blob):
    d = {}
    for tag,data in loopTLVBlocks(blob):
        d[tag] = data
    return d

def tlvToList(blob):
    return list(loopTLVBlocks(blob))
    
def loopTLVBlocks(blob):
    i = 0
    while i + 8 <= len(blob):
        tag = blob[i:i+4]
        length = struct.unpack(">L",blob[i+4:i+8])[0]
        data = blob[i+8:i+8+length]
        yield (tag,data)
        i += 8 + length

pl = "/Users/yielding/Desktop/work/af9583a0a2b1a8b9.plist"

pldict = plistlib.readPlist(pl)
keystore = KeyBag.createWithPlist(pldict)

# k835 = pldict["key835"].decode("hex")
# 
# blob   = pldict["KeyBagKeys"].data
# #print blob
# keybag = tlvToDict(blob)
# # print keybag["DATA"]
# # print "============================================"
# # print len(keybag.get("SIGN", ""))
# 
# #for tag, data in loopTLVBlocks(dd):
# #  print tag, " => ", data
# 
# CLASSKEY_TAGS = ["CLAS","WRAP","WPKY", "KTYP"]  #UUID
# for tag, data in loopTLVBlocks(keybag["DATA"]):
#   print tag, " = ", 
#   pp.pprint(data)
# 
# # uuid = None
# # wrap = None
# # type = None
# # currentClassKey = None
# # classKeys = {}
# # attrs = {}
# # for tag, data in loopTLVBlocks(keybag["DATA"]):
# #   if len(data) == 4:
# #     data = struct.unpack(">L", data)[0]
# #     
# #   if tag == "TYPE":
# #       print "TYPE = ", data
# #       if type > 2:
# #           print "FAIL: keybag type > 2"
# #   elif tag == "UUID" and uuid is None:
# #       uuid = data
# #       print "UUID = ", data
# #   elif tag == "WRAP" and wrap is None: 
# #       print "Wrap = ", data
# #       wrap = data
# #   elif tag == "UUID":
# #       if currentClassKey:
# #           classKeys[currentClassKey["CLAS"]] = currentClassKey
# #       currentClassKey = {"UUID": data}
# #   elif tag in CLASSKEY_TAGS:
# #       print tag, ", ",  data
# #       currentClassKey[tag] = data
# #   else:
# #       attrs[tag] = data
# #       print "attrs: ", tag, ", ",  data
# # 
