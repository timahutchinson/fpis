// Exercise 2.1
// Write a recursive function to get the nth Fibonacci number.
// The first two Fibonacci numbers are 0 and 1.
// The nth number is always the sum of the previous two — the sequence
// begins 0, 1, 1, 2, 3, 5.
// Your definition should use a local tail-recursive function.

def fib(n: Int): Int = {
  @annotation.tailrec
  def helper(n: Int, a: Int, b: Int): Int =
    if (n == 0) a
    else if (n == 1) b
    else helper(n-1, b, a+b)
  helper(n, 0, 1)
}

fib(10)

// Exercise 2.2
// Implement isSorted, which checks whether an Array[A] is
// sorted according to a given comparison function:
//
// def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean

def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
  @annotation.tailrec
  def loop(n: Int): Boolean = {
    if (n >= as.length-1) true
    else if (!ordered(as(n), as(n+1))) false
    else loop(n+1)
  }

  loop(0)
}

isSorted(Array(1,2,4,3), (a: Int, b: Int) => a <= b)