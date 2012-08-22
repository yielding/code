$:.unshift(File.dirname(__FILE__)) unless
  $:.include?(File.dirname(__FILE__)) or
  $:.include?(File.expand_path(File.dirname(__FILE__)))

require 'sky/book'
require 'sky/phone_numbers'

fname = "../resources/unused_efs2"
size  = File::size?(fname)

return 0 if size <= 0

File.open(fname) { |f|
}
