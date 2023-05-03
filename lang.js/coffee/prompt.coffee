#!/usr/bin/env coffee

stdin = process.openStdin()
stdin.setEncoding 'utf8'

inputCallback = null

stdin.on 'data', (input) -> inputCallback input
