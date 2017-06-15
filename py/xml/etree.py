#!/usr/bin/env python
# encoding: utf-8

import xml.etree.ElementTree as ET

doc  = ET.parse("test.xml")
root = doc.getroot()

for country in root.iter("country"):
    print "=" * 60

    print "Country : ", country.attrib["name"]
    print "Rank : ", country.findtext("rank")
    print "Year : ", country.findtext("year")
    for neighber in country.iter("neighbor"):
        print "Neighbor :", neighber.attrib
