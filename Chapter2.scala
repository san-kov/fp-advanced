// Exercise 2.1

// Find n-th number of fibonacci, using tail call recursion

def fib(n: Int): Int =
  def loop(current: Int, prevSum: Int, prevNum: Int): Int =

    val currentSum = prevSum + prevNum
    if current == n then currentSum
    else loop(current + 1, prevNum, currentSum)

  if n == 0 then 0 else if n == 1 then 1 else loop(2, 0, 1)

// Exercise 2.2

// Check whether array is sorted, given a comparison function

def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean =
  def loop(n: Int): Boolean =
    if n == as.length - 1
    then true
    else if gt(as(n + 1), as(n)) then loop(n + 1)
    else false

  if as.length == 0 || as.length == 1 then true else loop(0)

// Exercise 2.3

// Implement curry

def curry[A, B, C](f: (A, B) => C): A => (B => C) =
  (a: A) => (b: B) => f(a, b)

// Exercise 2.4

// Implement uncurry

def uncurry[A, B, C](f: A => B => C): (A, B) => C =
  (a: A, b: B) => f(a)(b)

// Exercise 2.5

// Implement function composition

def compose[A, B, C](f: B => C, g: A => B): A => C =
  (a: A) => f(g(a))
