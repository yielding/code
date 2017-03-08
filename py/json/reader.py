#!/usr/bin/env python

import json
import pprint

def load_from_file(fname):
    f = open(fname)
    config = json.loads(f.read())
    f.close()

    return config

config = load_from_file("config.json")

for app in config: 
    pprint.pprint(app)

