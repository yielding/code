require "hotcocoa"

include HotCocoa

application do |app|
  window :size => [100, 50] do |win|
    b = button :title => "hello"
    b.on_action { puts "hello"}
    win << b
    win.will_close {  puts "by"; exit }
  end
end