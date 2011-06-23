#!/usr/bin/env ruby -wKU

f = File.open("5_Manifest.mbdb", "rb")
f.seek(0, IO::SEEK_SET)
p f.read(6)
