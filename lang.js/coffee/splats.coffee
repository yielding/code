#!/usr/bin/env coffee

refine = (wheat, chaff...) ->
    console.log "The best: #{wheat}"
    console.log "The rest: #{chaff.join(', ')}"

refine 'great', 'not bad', 'so-so', 'meh'
