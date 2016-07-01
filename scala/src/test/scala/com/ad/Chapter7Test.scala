package com.ad

import java.util.concurrent.Executors

import org.scalatest._
import Chapter7.Par._;

class Chapter7Test extends FlatSpec with Matchers {

  // Exercise 7.1
  "map2" should "work combine two pars" in {
    (map2 (unit(1), unit(2)))(_+_) (Executors.newFixedThreadPool(1)).get should be (3)
  }

  // Exercise 7.1.a
  "sums" should "work on non empty sequences" in {
    (Chapter7.Par.sum(Seq(1,2,3,4,5)))(Executors.newFixedThreadPool(3)).get should be (15)
  }

  // Exercise 7.4
  "asyncF" should "should fork the computation" in {
    asyncF {x:Int => x + 1}(1)(Executors.newFixedThreadPool(1)).get should be (2)
  }

  // Exercise 7.5
  "sequence" should "should sequence the computation in correct order for parallel computations" in {
    sequence(List(fork(unit(1)),fork(unit(2)),fork(unit(3))))(Executors.newFixedThreadPool(2)).get should be  (List(1,2,3))
  }

  // Exercise 7.5
  "sequence" should "should sequence the computation" in {
    sequence(List(unit(1),unit(2),unit(3)))(Executors.newFixedThreadPool(1)).get should be  (List(1,2,3))
  }

  // Exercise 7.6
  "parFilter" should "should filter and preserve order and structure" in {
    parFilter(List(1,2,3)){x => x > 1}(Executors.newFixedThreadPool(2)).get should be (List(2,3))
  }

  // Exercise 7.6 parWordCount
  "parWordCount" should "work on non empty sequences" in {
    Chapter7.Par.parWordCount(List("A man", "A plan" , "panama","A man", "A plan" , "panama"))(Executors.newFixedThreadPool(4)).get should be (10)
  }

  //Exercise 7.6 parWordCount
  "fork" should "work for fixed thread pool executor" in {
    fork(fork(fork(fork(unit(1))))) (Executors.newFixedThreadPool(4)).get should be (1)
  }

  // Exercise 7.11
  "choiceN" should "should choose the correct parallel computation" in {
    choiceN(fork(unit(0)))(List(unit(1),unit(2),unit(3)))(Executors.newFixedThreadPool(1)).get should be (1)
  }

  "choiceAsChoiceN" should "should choose the correct parallel computation when condition is true" in {
    choice(unit(true))(unit(1),unit(2))(Executors.newFixedThreadPool(1)).get should be (1)
  }

  "choiceAsChoiceN" should "should choose the correct parallel computation when condition is false" in {
    choice(unit(false))(unit(1),unit(2))(Executors.newFixedThreadPool(1)).get should be (2)
  }

  "choiceMap" should "should choose the correct parallel computation for given key" in {
    choiceMap(unit(true))(Map(true -> unit(1), false ->unit(2)))(Executors.newFixedThreadPool(1)).get should be (1)
  }

  "choiceMap" should "should fail for non existent key" in {
    intercept[java.util.NoSuchElementException] {
      choiceMap(unit(false))(Map(true -> unit(1)))(Executors.newFixedThreadPool(1)).get should be(1)
    }
  }

  //7.13
  "chooser" should "should choose correct implementation" in {
   chooser(unit(1)){x => unit(2)}(Executors.newFixedThreadPool(1)).get should be(2)
  }

  //7.14
  "join" should "return results of inner computation" in {
    join(unit(unit(2)))(Executors.newFixedThreadPool(1)).get should be(2)
  }




}