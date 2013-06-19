#!/usr/bin/env ruby2.0
#-*- coding:utf-8 -*-
# encoding: utf-8

a = "leech kamin 이창하공장"

if a =~/이창하([ㄱ-ㅎ|가-힣]+.*)/
  puts $1
end
