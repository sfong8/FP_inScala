def fib(n:Int): Int = {
  def go(n:Int, acc : Int) : Int = {
    if (n==0)
      0
      else if (n==1)
        acc
    else
      go(n-1,acc) + go(n-2,acc)


  }
  go(n,1)
}

fib(6)