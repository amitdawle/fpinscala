package com.ad

import org.scalatest._

class Chapter5Test extends FlatSpec with Matchers {

  // Exercise 5.1
  "toList" should "work on empty streams" in {
    Chapter5.Stream.empty.toList  should be (Nil)
  }

  "toList" should "work on non-empty streams" in {
    Chapter5.Stream.apply(2,3,4).toList  should be (List(2,3,4))
  }

}