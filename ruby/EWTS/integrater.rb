class SSSGenerator
  def initialize path
    @sss   = Hash.new { |h, k| h[k] = Array.new }
    @path  = path
    @names = []
  end

  def read_sss_template_in sfile
    File.readlines(sfile).each do |line| 
      @sss[line.strip!] = Array.new 
    end
  end
           
  def load_sss_files
    Dir.foreach(@path) do |name|
      next if name.length < 8
      @names.push(name) if name.end_with?("sss.csv")
    end
  end

  def integrate_to out_file
    @names.each do |name|
      File.readlines(name).each do |line|  
        arr = line.strip!.split(",")
        key = arr[0]
        1.upto(arr.length-1) { |index| @sss[key].push(arr[index]) }
      end

      File.open(out_file, "w") do |file|
        @sss.each do |item|
          key = item[0]
          val = item[1].join
          file.printf("%s, %s\n", key, val)
        end
      end
    end
  end
end

if __FILE__ == $0
  path = ARGV.length > 1 ? ARGV[0] : Dir.pwd

  p path

  generator = SSSGenerator.new(path)
  begin
    sfile = "c:/trace/toss_sss.txt"
    generator.read_sss_template_in(sfile)
    generator.load_sss_files
    generator.integrate_to "c:/trace/res.csv"
  rescue Exception
  ensure
  end
end

