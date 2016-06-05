package com.ad

import org.scalatest._

class Chapter5Test extends FlatSpec with Matchers {

  // Exercise 5.1
  "toList" should "work on empty streams" in {
    Chapter5.Stream.empty.toList  should be (Nil)
  }

  "toList" should "work on non-empty streams" in {
    Chapter5.Stream.apply(2,3,4).toList should be (List(2,3,4))
  }


  "take" should "work on empty streams" in {
    Chapter5.Stream.empty.take(3).toList should be (Nil)
  }

  "take" should "work on non-empty streams when n < stream size" in {
    Chapter5.Stream.apply(2,3,4).take(1).toList should be (List(2))
  }

  "take" should "work on non-empty streams when n == stream size" in {
    Chapter5.Stream.apply(2,3,4).take(3).toList should be (List(2,3,4))
  }

  "take" should "work on non-empty streams when n > stream size" in {
    Chapter5.Stream.apply(2,3,4).take(5).toList should be (List(2,3,4))
  }

  "take" should "work on streams when n < 0" in {
    Chapter5.Stream.apply(2,3,4).take(-5).toList should be (Nil)
  }


  "drop" should "work on empty streams" in {
    Chapter5.Stream.empty.drop(3).toList should be (Nil)
  }

  "drop" should "work on non-empty streams when n < stream size" in {
    Chapter5.Stream.apply(2,3,4).drop(1).toList should be (List(3,4))
  }

  "drop" should "work on non-empty streams when n == stream size" in {
    Chapter5.Stream.apply(2,3,4).drop(3).toList should be (Nil)
  }

  "drop" should "work on non-empty streams when n > stream size" in {
    Chapter5.Stream.apply(2,3,4).drop(5).toList should be (Nil)
  }

  "drop" should "work on streams when n < 0" in {
    Chapter5.Stream.apply(2,3,4).drop(-5).toList should be (List(2,3,4))
  }
}