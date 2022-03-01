#!/usr/bin/env ruby

require "numeric_ext"

p (9183..9999).select { |e| e.concat(2 * e).pandigital? }
              .map    { |e| e.concat(2 * e) }
              .max
