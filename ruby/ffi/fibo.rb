#!/usr/bin/env ruby

require "ffi"

module FfiCustomTest
  extend FFI::Library
  ffi_lib 'c'
  ffi_lib './ffi_test.so'
  attach_function :ffi_pow, [ :int, :int ], :int
  attach_function :ffi_factorial, [ :int ], :int
  attach_function :ffi_fibonacci, [ :int ], :int
end

puts FfiCustomTest.ffi_factorial(5)
puts FfiCustomTest.ffi_fibonacci(9)
puts FfiCustomTest.ffi_pow(2, 10)
