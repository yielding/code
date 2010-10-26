require 'win32ole'

class String
  def to_kor
    return self.encode("euc-kr", "iso-8859-1")
  end
end

class EWTSAutomator
  def initialize(path_to_file)
    @word = WIN32OLE.new('word.application')
    @word.visible = false
    @word.documents.open(path_to_file)

    @file = path_to_file
    @data = []
    @reqs = Hash.new { |h, k| h[k] = Array.new }
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
      line.delete!("{").delete!("}")
      # [SK-KEWTS-TIA-HRS-001 SK-KEWTS-TOSS-SSS-001 SK-KEWTS-TOSS-SSS-002]

      puts line.gsub!(/ +/, ' ')
      @data.push(line.split)
      @word.selection.start = @word.selection.end + 1
    end
  end

  def collect_trace
    puts "begin MS Word automation"
    collect_data

    @data.each do |line|  
      # line = [SK-KEWTS-TIA-HRS-001 SK-KEWTS-TOSS-SSS-001 SK-KEWTS-TOSS-SSS-002]
      1.upto(line.length-1) { |sss| @reqs[line[sss]].push(line[0]) }
    end
  end

  def save_sss(to_screen=true)
    puts "\ngenerating sss..."
    File.open("#{@file}.sss.csv", "w") { |f|
      puts "--- SSS "

      @reqs.sort { |a, b| a[0] <=> b[0] }.each { |line| 
          printf("%s, %s\n", line[0], line[1].join(" ")); 
        f.printf("%s, %s\n", line[0], line[1].join(" ")); 
      }
    }
  end

  def save_req(to_screen=true)
    puts "\ngenerating reqs..."
    File.open("#{@file}.req.csv", "w") { |f|
      puts "--- Reqs "
      @data.each.sort { |a, b| a[0] <=> b[0] }.each { |line| 
        key = line[0]; line.shift
        res = line.join(" ")
          printf("%s, %s\n", key, res); 
        f.printf("%s, %s\n", key, res)
      }
    }
  end

  def quit
    @word.quit
  end

end

if __FILE__ == $0
  if ARGV.length < 1
    puts "wrong argument: ruby word_test.rb <file_name> "
    exit
  end

  begin
    ifile = "c:/trace/#{ARGV[0]}"
    automator = EWTSAutomator.new(ifile)
    automator.collect_trace
    automator.save_sss
    automator.save_req
  rescue Exception
    #
  ensure
    automator.quit
  end
end
