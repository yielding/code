class Fibo
  @@cache = [0, 1, 1]
  def Fibo.fibo(n)
    return @@cache[n] unless @@cache[n].nil?
    @@cache[n] = Fibo.fibo(n-1) + Fibo.fibo(n-2)
  end
end

2.upto(5000) { |p|
  if Fibo.fibo(p).to_s.size >= 1000
    puts "answer: #{p}"
    exit
  end
}
