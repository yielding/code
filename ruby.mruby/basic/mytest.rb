t = MyTest.new

1.upto(10) { |e| t.add(e) }

p t.to_s

config = JSON::parse '{"foo": "bar"}'
p config["foo"]
