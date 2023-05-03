require 'pp'

include OpenEye

#puts $:

# C++와 루비에서 동시에 접근 가능한 자료구조 생성
OpenEye::use_alias("rfid")

#RFSignal.find_by_tid(1).each do |s| 
#  puts "#{s.tid} #{s.rid} #{s.rssi} #{s.signal_time} #{s.status}" 
#end
#
## below tested OK!
#signal = RFSignal.create(2, 1, 199, "20080913121032", 1)
#signal.save
#signal.remove
#
#RFSignal.delete_by_tid(2);
#RFSignal.delete_all();

RFSignal.find_all.each do |s| 
  puts "#{s.tid} #{s.rid} #{s.rssi} #{s.signal_time} #{s.status}" 
end
#Reader.find_all.each   do |r| puts "#{r.name} #{r.location}" end
#Parent.find_all.each   do |p| puts "#{p.name} #{p.email} #{p.phone_number}" end
#Student.find_all.each do |s| puts "#{s.pid} #{s.name}" end

#puts "점심 먹은 사람"; had_lunch = []
#RFSignal.find_all.each do |signal|
#  student = signal.student
#  had_lunch << student.id if student.have_lunch
#end
#
#students = Student.find_all
#had_lunch.uniq.each do |st|
#  s = students[st]
#  puts "#{s.name} student had lunch for #{s.lunch_time} minutes"
#end
