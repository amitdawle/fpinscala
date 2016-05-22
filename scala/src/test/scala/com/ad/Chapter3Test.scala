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
    tail(Cons(1, Nil)) should be (Nil)
  }

  // Exercise 3.2
  "tail" should "return 1 element list for list with two elements" in {
    tail(Cons(1, Cons(2, Nil))) should be (Cons(2, Nil))
  }

  // Exercise 3.3
  "setHead" should "empty list should set a new head" in {
     setHead(1, Nil) should be (Cons (1, Nil))
  }

  // Exercise 3.2
  "tail" should "return replace head of a list " in {
    setHead(1, Cons(2, Nil)) should be (Cons (1, Nil))
  }

  // Exercise 3.3 drop
  "drop" should " should drop one element for list with one element" in {
    drop(Cons(2, Nil) , 1) should be (Nil)
  }

  "drop" should " should drop one element for list with more than one element" in {
    drop(Cons(2, Cons (3, Nil)) , 1) should be (Cons (3, Nil))
  }


  "drop" should " should return empty list for empty list (drop empty list differs in behaviour to tail empty list) " in {
    drop(Nil, 1) should be (Nil)
  }

  "drop" should " return empty list when n is greater than length of the list" in {
    drop(Cons(2, Cons (3, Nil)) , 10) should be (Nil)
  }

  // 3.4
  "tailWithDrop" should "throw exception for empty list (this is different to drop on empty list)" in {
    intercept[IllegalArgumentException] {
      tailWithDrop(Nil)
    }
  }
  // 3.4
  "tailWithDrop" should "return Nil for list with one element" in {
    tailWithDrop(Cons(1, Nil)) should be (Nil)
  }

  // 3.4
  "tailWithDrop" should "return List of one element for list with two elements" in {
    tailWithDrop(Cons(1, Cons(2, Nil))) should be (Cons(2, Nil))
  }


}
