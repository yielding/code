#!/usr/bin/env ruby

require 'lmdb'

path = "."
env = LMDB.new(path)

maindb = env.database
subdb = env.database('subdb', create: true)

maindb['key'] = 'value'

env.transaction do
  maindb['key'] = 'value'
  subdb['key'] = 'value'
end

env.close
