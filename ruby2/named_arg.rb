#!/usr/bin/env ruby2.0

def wrap(string, before:"<", after: ">")
  "#{before}#{string}#{after}"
end

# optional
p wrap("foo")                                  #=> "<foo>"
# one or the other
p wrap("foo", before: "#<")                    #=> "#<foo>"
p wrap("foo", after: "]")                      #=> "<foo]"
# order not important
p wrap("foo", after: "]", before: "[")         #=> "[foo]"

# double splat to capture all keyword arguments, or use as hash as keyword
# arguments
def capture(**opts)
  opts
end
p capture(foo: "bar")                          #=> {:foo=>"bar"}

# keys must be symbols
opts = {:before => "(", :after => ")"}
p wrap("foo", **opts)                          #=> "(foo)"

# the old hash style syantax is still accepted for keyword arguments
p wrap("foo", :before => "{", :after => "}")   #=> "{foo}"
