// Exercise 2.1

// Find n-th number of fibonacci, using tail call recursion

def fib(n: Int): Int =
  def loop(current: Int, prevSum: Int, prevNum: Int): Int =

    val currentSum = prevSum + prevNum
    if current == n then currentSum
    else loop(current + 1, prevNum, currentSum)

  if n == 0 then 0 else if n == 1 then 1 else loop(2, 0, 1)
