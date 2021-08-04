
class Fibo
  def Fibo.fibo(n)
    return 0 if n <= 0
    return 1 if n == 1 or n == 2
    return fibo(n-1) + fibo(n-2) if n >=3
  end                        

  def Fibo.add(a, b)
    return a + b
  end

  def Fibo.mul(a, b)
    6
  end
end
