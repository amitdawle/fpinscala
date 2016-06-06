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


  "takeWhile" should "work on empty streams" in {
    Chapter5.Stream.empty.takeWhile{ x:Int => x > 0 }.toList should be (Nil)
  }

  "takeWhile" should "work on non-empty streams when all elements satisfy p. " in {
    Chapter5.Stream.apply(2,3,4).takeWhile{ x:Int => x > 0 }.toList should be (List(2,3,4))
  }

  "takeWhile" should "work on non-empty streams when no elements satisfy p. " in {
    Chapter5.Stream.apply(2,3,4).takeWhile{ x:Int => x > 20 }.toList should be (List())
  }

  "takeWhile" should "work on non-empty streams when some elements satisfy p. " in {
    Chapter5.Stream.apply(2,3,4).takeWhile{ x:Int => x <= 3 }.toList should be (List(2,3))
  }

  "takeWhile" should "work on non-empty streams and should not take more elements once an element does not satisfy p even " +
    " other elements that satisfy p exist. " in {
    Chapter5.Stream.apply(2,3,4,1,0).takeWhile{ x:Int => x <= 3 }.toList should be (List(2,3))
  }


  "forAll" should "work on empty streams." in {
    Chapter5.Stream.empty.forAll{ x:Int => x > 20 } should be (true)
  }

  "forAll" should "work on non-empty streams when no elements satisfy p. " in {
    Chapter5.Stream.apply(2,3,4).forAll{ x:Int => x > 20 } should be (false)
  }

  "forAll" should "work on non-empty streams when some elements satisfy p. " in {
    Chapter5.Stream.apply(2,3,4).forAll{ x:Int => x < 4 } should be (false)
  }

  "forAll" should "work on non-empty streams when all elements satisfy p. " in {
    Chapter5.Stream.apply(2,3,4).forAll{ x:Int => x > 0 } should be (true)
  }

  "takeWhileWithFoldRight" should "work on empty streams" in {
    Chapter5.Stream.empty.takeWhileWithFoldRight{ x:Int => x > 0 }.toList should be (Nil)
  }

  "takeWhileWithFoldRight" should "work on non-empty streams when all elements satisfy p. " in {
    Chapter5.Stream.apply(2,3,4).takeWhileWithFoldRight{ x:Int => x > 0 }.toList should be (List(2,3,4))
  }

  "takeWhileWithFoldRight" should "work on non-empty streams when no elements satisfy p. " in {
    Chapter5.Stream.apply(2,3,4).takeWhileWithFoldRight{ x:Int => x > 20 }.toList should be (List())
  }

  "takeWhileWithFoldRight" should "work on non-empty streams when some elements satisfy p. " in {
    Chapter5.Stream.apply(2,3,4).takeWhileWithFoldRight{ x:Int => x <= 3 }.toList should be (List(2,3))
  }

  "takeWhileWithFoldRight" should "work on non-empty streams and should not take more elements once an element does not satisfy p even " +
    " other elements that satisfy p exist. " in {
    Chapter5.Stream.apply(2,3,4,1,0).takeWhileWithFoldRight{ x:Int => x <= 3 }.toList should be (List(2,3))
  }


  "headOptionWithFoldRight" should "work on empty streams. " in {
    Chapter5.Stream.empty.headOptionWithFoldRight should be (None)
  }

  "headOptionWithFoldRight" should "work on non empty streams. " in {
    Chapter5.Stream.apply(2,3,4,1,0).headOptionWithFoldRight should be (Some(2))
  }

  "map" should "work on empty streams. " in {
    Chapter5.Stream.empty[Int].map( x => x ).toList should be (Nil)
  }

  "map" should "work on non-empty streams. " in {
    Chapter5.Stream.apply(1,2,3).map( x => x + 1 ).toList should be (List(2,3,4))
  }

  "filter" should "work on empty streams. " in {
    Chapter5.Stream.empty[Int].filter(x => x > 1).toList should be (Nil)
  }

  "filter" should "work on non-empty streams. " in {
    Chapter5.Stream.apply(1,2,3,0,4).filter(x => x > 1).toList should be (List(2,3,4))
  }

  "append" should "work on empty streams. " in {
    Chapter5.Stream.empty.append(Chapter5.Stream.empty).toList should be (Nil)
  }

  "append" should "work on when first stream is empty. " in {
    Chapter5.Stream.empty.append(Chapter5.Stream.apply(1,2,3)).toList should be (List(1,2,3))
  }

  "append" should "work on when second stream is empty. " in {
    Chapter5.Stream.apply(1,2,3).append(Chapter5.Stream.empty).toList should be (List(1,2,3))
  }

  "append" should "work on when both streams are non empty. " in {
    Chapter5.Stream.apply(1,2,3).append(Chapter5.Stream.apply(4,5)).toList should be (List(1,2,3,4,5))
  }

  "flatMap" should "work on empty stream. " in {
    Chapter5.Stream.empty[Int].flatMap(x => Chapter5.Stream.cons(x , Chapter5.Stream.empty)).toList should be (Nil)
  }

  "flatMap" should "work on non-empty stream. " in {
    Chapter5.Stream.apply(1,2,3).flatMap(x => Chapter5.Stream.cons(x , Chapter5.Stream.empty)).toList should be (List(1,2,3))
  }

  "constant" should "generate stream of ints. " in {
    Chapter5.Stream.constant(1).take(5).toList  should be (List(1,1,1,1,1))
  }

  "constant" should "generate stream of chars. " in {
    Chapter5.Stream.constant('c').take(5).toList  should be (List('c','c','c','c','c'))
  }

  "from" should "generate stream of integers." in {
    Chapter5.Stream.from(1).take(5).toList  should be (List(1,2,3,4,5))
  }

  "fib" should "generate stream of fibonacci series." in {
    Chapter5.Stream.fib.take(7).toList  should be (List(0,1,1,2,3,5,8))
  }


  "unfold" should "generate stream of constant (constant(1)) ." in {
    Chapter5.Stream.unfold(1)(v => Some(v , v)).take(7).toList should be (List(1,1,1,1,1,1,1))
  }

  "unfold" should "generate stream of ascending numbers (from(n)) ." in {
    Chapter5.Stream.unfold(1)(v => Some(v , v + 1)).take(7).toList should be (List(1,2,3,4,5,6,7))
  }

  "unfold" should "generate stream of fibonacci series." in {
    Chapter5.Stream.unfold((0,1))(s => Some(s._1 , (s._2 , s._1 + s._2)) ).take(7).toList should be (List(0,1,1,2,3,5,8))
  }


  "mapWithUnfold" should "work on empty streams. " in {
    Chapter5.Stream.empty[Int].mapWithUnfold( x => x ).toList should be (Nil)
  }

  "mapWithUnfold" should "work on non-empty streams. " in {
    Chapter5.Stream.apply(1,2,3).mapWithUnfold( x => x + 1 ).toList should be (List(2,3,4))
  }


  "takeWithUnfold" should "work on empty streams" in {
    Chapter5.Stream.empty.takeWithUnfold(3).toList should be (Nil)
  }

  "takeWithUnfold" should "work on non-empty streams when n < stream size" in {
    Chapter5.Stream.apply(2,3,4).takeWithUnfold(1).toList should be (List(2))
  }

  "takeWithUnfold" should "work on non-empty streams when n == stream size" in {
    Chapter5.Stream.apply(2,3,4).takeWithUnfold(3).toList should be (List(2,3,4))
  }

  "takeWithUnfold" should "work on non-empty streams when n > stream size" in {
    Chapter5.Stream.apply(2,3,4).takeWithUnfold(5).toList should be (List(2,3,4))
  }

  "takeWithUnfold" should "work on streams when n < 0" in {
    Chapter5.Stream.apply(2,3,4).takeWithUnfold(-5).toList should be (Nil)
  }


  "takewhileWithUnfold" should "work on empty streams" in {
    Chapter5.Stream.empty.takewhileWithUnfold{ x:Int => x > 0 }.toList should be (Nil)
  }

  "takewhileWithUnfold" should "work on non-empty streams when all elements satisfy p. " in {
    Chapter5.Stream.apply(2,3,4).takewhileWithUnfold{ x:Int => x > 0 }.toList should be (List(2,3,4))
  }

  "takewhileWithUnfold" should "work on non-empty streams when no elements satisfy p. " in {
    Chapter5.Stream.apply(2,3,4).takewhileWithUnfold{ x:Int => x > 20 }.toList should be (List())
  }

  "takewhileWithUnfold" should "work on non-empty streams when some elements satisfy p. " in {
    Chapter5.Stream.apply(2,3,4).takewhileWithUnfold{ x:Int => x <= 3 }.toList should be (List(2,3))
  }

  "takewhileWithUnfold" should "work on non-empty streams and should not take more elements once an element does not satisfy p even " +
    " other elements that satisfy p exist. " in {
    Chapter5.Stream.apply(2,3,4,1,0).takewhileWithUnfold{ x:Int => x <= 3 }.toList should be (List(2,3))
  }

  "zipWith" should "work on empty streams." in {
    Chapter5.Stream.empty[Int].zipWith(Chapter5.Stream.empty[Int])(_+_).toList should be (List())
  }

  "zipWith" should "work on non-empty streams." in {
    Chapter5.Stream.apply(2,3,4,1,0).zipWith(Chapter5.Stream.apply(2,3,4,1,0))(_+_).toList should be (List(4,6,8,2,0))
  }

  "zipWith" should "work when this stream is empty." in {
    Chapter5.Stream.empty[Int].zipWith(Chapter5.Stream.apply(2,3,4,1,0))(_+_).toList should be (Nil)
  }

  "zipWith" should "work when other stream is empty." in {
    Chapter5.Stream.apply(2,3,4,1,0).zipWith(Chapter5.Stream.empty[Int])(_+_).toList should be (Nil)
  }

  "zipAll" should "work on empty streams." in {
    Chapter5.Stream.empty[Int].zipAll(Chapter5.Stream.empty[Int]).toList should be (List())
  }

  "zipAll" should "work on non-empty streams." in {
    Chapter5.Stream.apply(1,2).zipAll(Chapter5.Stream.apply(4,5)).toList should be (List((Some(1),Some(4)), (Some(2),Some(5))))
  }

  "zipAll" should "work when other stream is smaller than this stream." in {
    Chapter5.Stream.apply(1,2).zipAll(Chapter5.Stream.apply(4)).toList should be (List((Some(1),Some(4)), (Some(2),None)))
  }

  "zipAll" should "work when this stream is smaller than other stream." in {
    Chapter5.Stream.apply(1).zipAll(Chapter5.Stream.apply(4,5)).toList should be (List((Some(1),Some(4)), (None, Some(5))))
  }

  "zipAll" should "work when this stream is empty." in {
    Chapter5.Stream.empty.zipAll(Chapter5.Stream.apply(4)).toList should be (List((None,Some(4))))
  }

  "zipAll" should "work when other stream is empty." in {
    Chapter5.Stream.apply(1).zipAll(Chapter5.Stream.empty).toList should be (List((Some(1),None)))
  }


  "tails" should "should work on empty list." in {
    Chapter5.Stream.empty.tails.toList should be (List())
  }

  "tails" should "should work on non empty list." in {
    Chapter5.Stream.apply(1,2,3).tails.map(_.toList).toList should be (List(List(1, 2, 3), List(2, 3), List(3)))
  }

  "startsWith" should "should work when this stream and argument stream is empty." in {
    Chapter5.Stream.empty.startsWith(Chapter5.Stream.empty) should be (true)
  }

  "startsWith" should "should work when this stream is empty but argument stream is non empty." in {
    Chapter5.Stream.empty.startsWith(Chapter5.Stream.apply(1,2,3)) should be (false)
  }

  "startsWith" should "should work when this stream is non empty but argument stream is empty." in {
    Chapter5.Stream.apply(1,2,3).startsWith(Chapter5.Stream.empty) should be (true)
  }

  "startsWith" should "should work when this stream is non empty and argument stream is non empty and this stream starts with argument stream." in {
    Chapter5.Stream.apply(1,2,3).startsWith(Chapter5.Stream.apply(1,2)) should be (true)
  }

  "startsWith" should "should work when this stream same as argument stream." in {
    Chapter5.Stream.apply(1,2,3).startsWith(Chapter5.Stream.apply(1,2,3)) should be (true)
  }

  "startsWith" should "should work when this stream is smaller then argument stream." in {
    Chapter5.Stream.apply(1,2,3).startsWith(Chapter5.Stream.apply(1,2,3,4)) should be (false)
  }

  "scanRight" should "should work on empty list." in {
    Chapter5.Stream.empty[Int].scanRight(0)(_+_).toList should be (List())
  }

  "scanRight" should "should work on non-empty list." in {
    Chapter5.Stream.apply(1,2,3).scanRight(0)(_+_).toList should be (List(6,5,3))
  }

  "tailsWithUnfold" should "should work on empty list." in {
    Chapter5.Stream.empty.tailsWithScanRight.toList should be (List())
  }

  "tailsWithUnfold" should "should work on non empty list." in {
    Chapter5.Stream.apply(1,2,3).tailsWithScanRight.map(_.toList).toList should be (List(List(1, 2, 3), List(2, 3), List(3)))
  }


}