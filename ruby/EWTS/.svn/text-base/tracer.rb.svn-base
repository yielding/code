require 'win32ole'

# TODO
# 0. Automator Test ¹æ¾È
# 1. qualification
# 2. °¢ PUID ´ÙÀ½¿¡ enter ³Ö±â
# 3. KEWTS-SSS´Â sorting ÇÏÁö ¾Ê´Â´Ù.
#
class Object
  def deep_copy
    Marshal.load(Marshal.dump(self))
  end
end

class String
  def to_kor
    return self.encode("euc-kr", "iso-8859-1")
  end
end

class EWTSAutomator
  attr_reader :files

  def initialize(file)
    @data = []
    @reqs = Hash.new { |h, k| h[k] = Array.new }
    @path = "#{Dir.pwd}/#{file}"

    @word = WIN32OLE.new('word.application')
    @word.visible = false
    @word.documents.open(@path)
  end

  def change_pattern_of(pattern, start=1)
    suffix = pattern.split(/-/)[-1]
    base   = pattern.split(suffix)[0] 
    @word.selection.find.text = pattern 
    count = start
    while @word.selection.find.execute == true do
      @word.selection.typetext(base + sprintf("%03d", count))
      count += 1
    end
  end

  def find pattern
    @word.selection.find.text = pattern
    return @word.selection.find.execute == true
  end

  def collect_data
    # [{SK-KEWTS-TIA-HRS-001}, {SK-KEWTS-TOSS-SSS-001},, SK-KEWTS-TOSS-SSS-002  ]
    loop do
      return if not find("[")
      beg_ = @word.selection.start + 1
      @word.selection.MoveRight(1)
      @word.selection.MoveRight(1) until @word.selection.text.to_kor.end_with?("]")

      @word.selection.start = beg_
      @word.selection.end

      line = @word.Selection.Text.to_kor.gsub(/,/, ' ')
      #line.delete!("{").delete!("}")
      line.delete!("{")
      line.gsub!(/}/, ' ')
      # [SK-KEWTS-TIA-HRS-001 SK-KEWTS-TOSS-SSS-001 SK-KEWTS-TOSS-SSS-002]

      line.gsub!(/ +/, ' ')
      puts line
      @data.push(line.split)
      @word.selection.start = @word.selection.end + 1
    end
  end

  def read_sss_in file
    File.readlines(file).map(&:chomp).each do |line| 
      @reqs[line] = [] 
    end
  end

  def collect_trace
    puts "begin MS Word automation"

    collect_data

    @data.each do |line|  
      # line = ["SK-KEWTS-TIA-HRS-001", "SK-KEWTS-TOSS-SSS-001", "SK-KEWTS-TOSS-SSS-002", I]
      copy = line.deep_copy
      req  = copy.shift
      copy.each { |sss| @reqs[sss].push(req) }
    end
  end

  def save_sss(to_screen=true)
    puts "\ngenerating sss..."
    File.open("#{@path}.sss.csv", "w") do |f|
      puts "--- SSS "

      @reqs.sort { |a, b| a[0] <=> b[0] }.each do |line| 
        key, res = line.shift, line.join(" ")
        printf("%s, %s\n", key, res); f.printf("%s,%s\n", key, res)
      end
    end
  end

  def save_req(to_screen=true)
    puts "\ngenerating reqs..."
    File.open("#{@path}.req.csv", "w") do |f|
      puts "--- Reqs "
      @data.each.sort { |a, b| a[0] <=> b[0] }.each do |line| 
        key, res = line.shift, line.join(" ")
        printf("%s,%s\n", key, res); f.printf("%s,%s\n", key, res)
      end
    end
  end

  def quit
    @word.quit
  end
end

if __FILE__ == $0
  if ARGV.length < 2
    puts "usage: ruby #{$0} <sss_file> <file_name>"
    exit
  end

  tracer = EWTSAutomator.new(ARGV[1])
  begin
    sfile = "#{Dir.pwd}/#{ARGV[0]}"
    #tracer.change_pattern_of("KEWTS-SAS-SSM-SRS-xxx")
    tracer.read_sss_in(sfile)
    tracer.collect_trace
    tracer.save_sss
    tracer.save_req
  rescue Exception
    #
  ensure
    tracer.quit
  end
end
