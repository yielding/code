#!/usr/bin/env ruby19

p (1..20).reduce(1) { |res, n| res.lcm(n) }
