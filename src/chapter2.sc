// Exercise 2.1

def fib(n: Int): Int = {
  @annotation.tailrec
  def helper(n: Int, a: Int, b: Int): Int =
    if (n == 0) a
    else if (n == 1) b
    else helper(n-1, b, a+b)
  helper(n, 0, 1)
}

fib(10)