class Fibo
  def self.fibo(n)
    return 0 if n <= 0
    return 1 if [1, 2].include? n

    fibo(n - 1) + fibo(n - 2) if n >= 3
  end

  def self.add(a, b)
    a + b
  end

  def self.mul(a, b)
    a * b
  end
end
