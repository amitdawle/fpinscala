package com.ad

import org.scalatest._

import com.ad.Chapter7._


class Chapter7Test extends FlatSpec with Matchers {

  // Exercise 2.1
  "sum" should "produce expected value for large inputs" in {
    Par.sum( (1 to 10).seq )(java.util.concurrent.Executors.newFixedThreadPool(10)).get should be (55)
  }

}
