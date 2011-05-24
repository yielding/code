#!/usr/bin/env coffee

odd = (num) ->
    throw "#{num} is not a number" unless typeof num is 'number'
    throw "#{num} is not an integer" unless num is Math.round num
    throw "#{num} is not positive" unless num > 0

    num % 2 is 1

console.log odd 1
console.log odd 2
