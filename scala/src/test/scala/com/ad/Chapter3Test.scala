package com.ad


import com.ad.Chapter3._

import org.scalatest._

class Chapter3Test extends FlatSpec with Matchers {

  // Exercise 3.2
  "tail" should "throw error on empty list" in {
    intercept[IllegalArgumentException] {
      tail(Nil)
    }
  }

  // Exercise 3.2
  "tail" should "return empty list for list with one element" in {
    tail(Cons(1, Nil)) should be(Nil)
  }

  // Exercise 3.2
  "tail" should "return 1 element list for list with two elements" in {
    tail(Cons(1, Cons(2, Nil))) should be(Cons(2, Nil))
  }

  // Exercise 3.3
  "setHead" should "empty list should set a new head" in {
    setHead(1, Nil) should be(Cons(1, Nil))
  }

  // Exercise 3.2
  "tail" should "return replace head of a list " in {
    setHead(1, Cons(2, Nil)) should be(Cons(1, Nil))
  }

  // Exercise 3.3 drop
  "drop" should " should drop one element for list with one element" in {
    drop(Cons(2, Nil), 1) should be(Nil)
  }

  "drop" should " should drop one element for list with more than one element" in {
    drop(Cons(2, Cons(3, Nil)), 1) should be(Cons(3, Nil))
  }


  "drop" should " should return empty list for empty list (drop empty list differs in behaviour to tail empty list) " in {
    drop(Nil, 1) should be(Nil)
  }

  "drop" should " return empty list when n is greater than length of the list" in {
    drop(Cons(2, Cons(3, Nil)), 10) should be(Nil)
  }

  // 3.4
  "tailWithDrop" should "throw exception for empty list (this is different to drop on empty list)" in {
    intercept[IllegalArgumentException] {
      tailWithDrop(Nil)
    }
  }
  // 3.4
  "tailWithDrop" should "return Nil for list with one element" in {
    tailWithDrop(Cons(1, Nil)) should be(Nil)
  }

  // 3.4
  "tailWithDrop" should "return List of one element for list with two elements" in {
    tailWithDrop(Cons(1, Cons(2, Nil))) should be(Cons(2, Nil))
  }

  // -- Exercise 3.5
  "dropWhile" should "return Nil for empty List" in {
    dropWhile(Nil, { x: Int => false }) should be(Nil)
  }

  // -- Exercise 3.5
  "dropWhile" should "return Nil where for all x => f(x) is true" in {
    dropWhile(Cons(2, Cons(3, Cons(4, Nil))), { x: Int => true }) should be(Nil)
  }

  // -- Exercise 3.5
  "dropWhile" should "return the same list where for all x => f(x) is false" in {
    dropWhile(Cons(2, Cons(3, Cons(4, Nil))), { x: Int => false }) should be(Cons(2, Cons(3, Cons(4, Nil))))
  }

  // -- Exercise 3.5
  "dropWhile" should "return a (Cons(2, Cons(0, Cons (4, Nil))) list when for (Cons 0, (Cons(2, Cons(3, Cons (4, Nil)))) for f(x) => x <= 0  " in {
    dropWhile(Cons(0, Cons(2, Cons(0, Cons(4, Nil)))), { x: Int => x <= 0 }) should be(Cons(2, Cons(0, Cons(4, Nil))))
  }

  // -- Exercise 3.6
  "init" should "return Nil for empty List" in {
    init(Nil) should be(Nil)
  }

  // -- Exercise 3.6
  "init" should "return Nil for List with one element" in {
    init(Cons(2, Nil)) should be(Nil)
  }

  // -- Exercise 3.6
  "init" should "return Cons 2 Nil for List (Cons(2, Cons(3, Nil)) " in {
    init(Cons(2, Cons(3, Nil))) should be(Cons(2, Nil))
  }

  // -- Exercise 3.6
  "takeWhile" should "return empty for empty list." in {
    takeWhile(Nil: List[Int])(x => x > 0) should be(Nil)
  }

  // -- Exercise 3.6
  "takeWhile" should "return empty for list with elements but on match." in {
    takeWhile(Cons(-2, Cons(-3, Nil)))(x => x > 0) should be(Nil)
  }

  // -- Exercise 3.6
  "takeWhile" should "return list with matching elements." in {
    takeWhile(Cons(-2, Cons(-3, Cons(4, Nil))))(x => x < 0) should be(Cons(-2, Cons(-3, Nil)))
  }

  // -- Exercise 3.6
  "takeWhile" should "stop taking more elements once it find a non matching element." in {
    takeWhile(Cons(-2, Cons(3, Cons(-4, Nil))))(x => x < 0) should be(Cons(-2, Nil))
  }

  // -- Exercise 3.7
  "foldRight" should " should compute sum correctly when folding function is sum." in {
    foldRight(Cons(-2, Cons(3, Cons(-4, Nil))))(0)((x, y) => x + y) should be(-3)
  }

  "foldRight" should " should compute difference correctly when folding function is minus." in {
    foldRight(Cons(-2, Cons(3, Cons(-4, Nil))))(0)((x, y) => x - y) should be(-9)
  }

  // -- Exercise 3.7
  "foldRight" should " with Cons function should just copy the list." in {
    foldRight(Cons(-2, Cons(3, Cons(-4, Nil))))(Nil: List[Int])((x, y) => Cons(x, y)) should be(Cons(-2, Cons(3, Cons(-4, Nil))))
  }

  // -- Exercise 3.7
  "foldRight" should " with Cons function and a Nil list should just return Nil." in {
    foldRight(Nil)(Nil: List[Int])((x, y) => Cons(x, y)) should be(Nil)
  }

  // Exercise 3.9
  "lengthWithFoldRight" should " work with an empty list." in {
    lengthWithFoldRight(Nil) should be(0)
  }

  // Exercise 3.9
  "lengthWithFoldRight" should " work with a non empty list." in {
    lengthWithFoldRight(Cons(2, Cons(3, Nil))) should be(2)
  }


  // -- Exercise 3.10
  "foldLeft" should " should compute sum correctly when folding function is sum." in {
    foldLeft(Cons(-2, Cons(3, Cons(-4, Nil))))(0)((x, y) => x + y) should be(-3)
  }

  "foldLeft" should " should compute difference correctly when folding function is minus." in {
    foldLeft(Cons(-2, Cons(3, Cons(-4, Nil))))(0)((t, x) => t - x) should be(3)
  }

  // -- Exercise 3.10
  "foldLeft" should " with Cons function should just reverse the list." in {
    foldLeft(Cons(-2, Cons(3, Cons(-4, Nil))))(Nil: List[Int])((t, x) => Cons(x, t)) should be(Cons(-4, Cons(3, Cons(-2, Nil))))
  }

  // -- Exercise 3.10
  "foldLeft" should " with Cons function and a Nil list should just return Nil." in {
    foldLeft(Nil)(Nil: List[Int])((x, y) => Cons(y, x)) should be(Nil)
  }

  // Exercise 3.11
  "sum" should " using fold should work correctly" in {
    sum(Cons(2, Cons(3, Cons(4, Nil)))) should be(9)
  }

  "sum" should " using fold on empty list should be 0" in {
    sum(Nil) should be(0)
  }

  "product" should " using fold should work correctly" in {
    product(Cons(2, Cons(-3, Cons(4, Nil)))) should be(-24)
  }

  "product" should " using fold on empty list should be 1" in {
    product(Nil) should be(1)
  }

  "length" should " using fold should work correctly" in {
    Chapter3.length(Cons(2, Cons(-3, Cons(4, Nil)))) should be(3)
  }

  "length" should " using fold on empty list should be 1" in {
    Chapter3.length(Nil) should be(0)
  }

  "reverse" should " using foldl on empty list should be empty" in {
    reverse(Nil) should be(Nil)
  }

  "reverse" should " using foldl should reverse the list" in {
    reverse(Cons(2, Cons(-3, Cons(4, Nil)))) should be(Cons(4, Cons(-3, Cons(2, Nil))))
  }


  //foldLeftWithFoldRight
  "foldLeftWithFoldRight" should " on empty list should be empty" in {
    foldLeftWithFoldRight(Nil)(Nil: List[Int])((t, x) => Cons(x, t)) should be(Nil)
  }

  "foldLeftWithFoldRight" should " using foldl should reverse the list" in {
    foldLeftWithFoldRight(Cons(-2, Cons(3, Cons(-4, Nil))))(Nil: List[Int])((t, x) => Cons(x, t)) should be(Cons(-4, Cons(3, Cons(-2, Nil))))
  }
}