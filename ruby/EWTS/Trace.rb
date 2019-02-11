#!/opt/local/bin/ruby -wKU
# encoding: utf-8

require "test/unit"
require "rbconfig"

include Config

require "win32ole" if CONFIG["build_os"] =~/mswin/

class Object
  def deep_copy
    Marshal.load(Marshal.dump(self))
  end
end

class String
  def to_kor
    return self.encode("euc-kr", "UTF-8")
  end
end

class Collector
  attr_reader :buffer
  def initialize
    @buffer = []
  end
end

class TextCollector < Collector
  def initialize(buf)
    @buffer = buf
  end

  def collect
    @buffer
  end
end

class FileCollector < Collector
  def initialize filename
  end

  def collect
    @buffer
  end
end

class WordCollector < Collector
  def initialize
  end

  def collect
    @buffer
  end
end

class ClipboardCollector < Collector
  def initialize
  end

  def collect
    @buffer
  end
end

class Tracer
  attr_reader :reqs
  attr_accessor :collector

  def initialize(collector=nil)
    @reqs = []
    @collector = collector
  end

  def read_sss_in(file)
    path  = "#{Dir.pwd + file}" 
    count = 0
    File.readlines(file).map(&:chomp).each do |line|
      @reqs[line] = []
      count += 1
    end
    count
  end  
  
  def check_format_of buffer
    next_state = { "[" => "]", "]" => "[" }
    expecting  = "["
    count = 0
    buffer.each_char do |ch| 
      next if not ["[", "]"].include?(ch)
      count += 1
      return count if expecting == next_state[ch]
      expecting = next_state[ch]
    end

    return expecting != "[" ? count : -1
  end

  #
  # last item I=inspection, D=Demo, A=Analisys, C=Custom 
  #           Default=I
  # [{SK-KEWTS-TIA-HRS-001}, {SK-KEWTS-TOSS-SSS-001},, SK-KEWTS-TOSS-SSS-002   ]
  # [{SK-KEWTS-TIA-HRS-001}, {SK-KEWTS-TOSS-SSS-001},, SK-KEWTS-TOSS-SSS-002, I]
  #
  def process buffer
    return false if buffer.empty? or check_format_of(buffer) != -1

    @reqs = []
    # watch out => /\[(.+?)\]/ non-greedy version
    buffer.scan(/\[(.+?)\]/).each do |item|
      item.each { |puid| 
        puid.gsub!(/\[|{|}|,/, ' ')
        puid.gsub!(/ +/, ' ')
        puid.strip!
        @reqs.push(puid.split.uniq)
      }
    end
    true
  end

  def sss
    specs = Hash.new { |h, k| h[k] = Array.new }
    @reqs.each do |req|  
      # req = ["SK-KEWTS-TIA-HRS-001", "SK-KEWTS-TOSS-SSS-001", "SK-KEWTS-TOSS-SSS-002"]
      cp  = req.deep_copy
      req = cp.shift
      cp.each { |sss| specs[sss].push(req) }
    end
    specs
  end

end

class TestTracer < Test::Unit::TestCase
  LINES = {:TOSS => 137, :SAS => 100 }
  
  def setup 
    @tracer = Tracer.new
  end
  
  def test_empty
    assert_equal(false, @tracer.process(""))
  end
                           
  def test_format_correct
    assert_equal(-1, @tracer.check_format_of("[]"))
    assert_equal(-1, @tracer.check_format_of("[][]"))
    assert_equal(-1, @tracer.check_format_of("[][][        ]"))
    assert_equal(-1, @tracer.check_format_of("[][][        ][                   ]"))
  end

  def test_format_not_correct
    assert_equal(1, @tracer.check_format_of("]"))
    assert_equal(1, @tracer.check_format_of("["))
    assert_equal(2, @tracer.check_format_of("[[]"))
    assert_equal(2, @tracer.check_format_of("[[]]"))
    res = @tracer.process "[{KEWTS-TOSS-SSS-003}"
    assert_equal(res, false)
  end

  def test_zero_req
    assert_equal(true, @tracer.process("[{KEWTS-TOSS-SSS-003}]"))
    assert_equal(@tracer.reqs, [["KEWTS-TOSS-SSS-003"]])
  end

  def test_one_req_one_sss_text_collector
    t1 = TextCollector.new("[{KEWTS-TOSS-TPC-HRS-001}, {KEWTS-TOSS-SSS-003}]")
    assert_equal(true, @tracer.process(t1.collect))
    assert_equal(@tracer.reqs, [["KEWTS-TOSS-TPC-HRS-001", "KEWTS-TOSS-SSS-003"]])
  end

  def test_one_req_one_sss
    i1 = "[{KEWTS-TOSS-TPC-HRS-001}, {KEWTS-TOSS-SSS-003}]"
    t1 = TextCollector.new("[{KEWTS-TOSS-TPC-HRS-001}, {KEWTS-TOSS-SSS-003}]")
    assert_equal(true, @tracer.process(i1))
    assert_equal(true, @tracer.process(t1.collect))
    assert_equal(@tracer.reqs, [["KEWTS-TOSS-TPC-HRS-001", "KEWTS-TOSS-SSS-003"]])
   
    i2 = "이창하 [{KEWTS-TOSS-TPC-HRS-001}, {KEWTS-TOSS-SSS-003}] 바보"
    assert_equal(true, @tracer.process(i2))
    assert_equal(@tracer.reqs, [["KEWTS-TOSS-TPC-HRS-001", "KEWTS-TOSS-SSS-003"]])
  end
  
  def test_one_req_two_sss
    i1 = "[{KEWTS-TOSS-TPC-HRS-001}, {KEWTS-TOSS-SSS-003}KEWTS-TOSS-SSS-004,,,,]"
    assert_equal(true, @tracer.process(i1))
    assert_equal(@tracer.reqs, 
      [["KEWTS-TOSS-TPC-HRS-001", "KEWTS-TOSS-SSS-003", "KEWTS-TOSS-SSS-004"]])
  end

  def test_one_req_two_sss_with_dup
    i1 = "[{KEWTS-TOSS-TPC-HRS-001}, {KEWTS-TOSS-SSS-003}KEWTS-TOSS-SSS-003,,,,]"
    assert_equal(true, @tracer.process(i1))
    assert_equal(@tracer.reqs, 
      [["KEWTS-TOSS-TPC-HRS-001", "KEWTS-TOSS-SSS-003"]])
  end

  def test_two_req_two_sss
    i1 =<<-HERE
      [{KEWTS-TOSS-TPC-HRS-001}, KEWTS-TOSS-SSS-003 KEWTS-TOSS-SSS-003,,,,]
      [{KEWTS-TOSS-TPC-HRS-002}, KEWTS-TOSS-SSS-003 KEWTS-TOSS-SSS-004,,,,]
    HERE
    assert_equal(true, @tracer.process(i1))
    assert_equal(@tracer.reqs, 
      [["KEWTS-TOSS-TPC-HRS-001", "KEWTS-TOSS-SSS-003"],
       ["KEWTS-TOSS-TPC-HRS-002", "KEWTS-TOSS-SSS-003", "KEWTS-TOSS-SSS-004"]
       ])
  end

  def test_one_sss
    i1 =<<-HERE
      [{KEWTS-TOSS-TPC-HRS-001}, KEWTS-TOSS-SSS-003]
    HERE
    assert_equal(true, @tracer.process(i1))
    assert_equal(
      {"KEWTS-TOSS-SSS-003"=> ["KEWTS-TOSS-TPC-HRS-001"]},
      @tracer.sss
    )
    i2 =<<-HERE
      Sss

      이창하는 바보다. [{KEWTS-TOSS-TPC-HRS-001}, {KEWTS-TOSS-SSS-003}]
      이창하는 바보다. [{KEWTS-TOSS-TPC-HRS-003} {KEWTS-TOSS-SSS-001} KEWTS-TOSS-SSS-005]
      이창하는 바다. [{KEWTS-TOSS-TPC-HRS-002} {KEWTS-TOSS-SSS-004}]


      하하하
      KEWTS-TOSS-TPC-HRS-xxx
      KEWTS-TOSS-TPC-HRS-xxx
    HERE

    assert_equal(true, @tracer.process(i2))
    assert_equal(
      {
      "KEWTS-TOSS-SSS-001"=> ["KEWTS-TOSS-TPC-HRS-003"],
      "KEWTS-TOSS-SSS-003"=> ["KEWTS-TOSS-TPC-HRS-001"],
      "KEWTS-TOSS-SSS-004"=> ["KEWTS-TOSS-TPC-HRS-002"],
      "KEWTS-TOSS-SSS-005"=> ["KEWTS-TOSS-TPC-HRS-003"]
      },
      @tracer.sss
    )

    assert_equal(
       [["KEWTS-TOSS-TPC-HRS-001", "KEWTS-TOSS-SSS-003"],
        ["KEWTS-TOSS-TPC-HRS-003", "KEWTS-TOSS-SSS-001", "KEWTS-TOSS-SSS-005"],
        ["KEWTS-TOSS-TPC-HRS-002", "KEWTS-TOSS-SSS-004"]],
      @tracer.reqs
    )
  end

end  

if __FILE__ == $0
  puts "hello"               
end
