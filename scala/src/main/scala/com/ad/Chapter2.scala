package com.ad

object Chapter2 {

  // Exercise 2.1
  // nth Fibonacci number

  def fib(n: Int) : Int = {
    require(n >= 1 , "n should be greater than 0")
    def go(i:Int, a:Int, b:Int) : Int = i match {
      case 0 => a
      case _ => go( i - 1, b, a + b)
    }
    go(n - 1, 0, 1)
  }

  // Exercise 2.2
  // Unlike haskell, the pattern matching did not work correctly for scala arrays
  // ideally a much cleaner solution (conceptually) is the one in comments below
  /*
    def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = as match {
    case Array() => true
    case Array(a) => true
    case Array(a, b) => ordered(a, b)
    case Array(a, b, as@_*) => ordered(a, b) && isSorted(as, ordered)
  }
 */
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = as.length match {
    case 0 => true
    case 1 => true
    case 2 => ordered(as(0), as(1))
    case _ => ordered(as(0), as(1)) && isSorted( as.drop(2), ordered)
  }


}
