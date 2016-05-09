package ad

import org.scalatest._

import com.ad.Chapter2._

import scala.Function

class Chapter2Test extends FlatSpec with Matchers {

  // Exercise 2.1
  "fib" should "produce expected value for small inputs" in {
    fib(1) should be (0)
    fib(2) should be (1)
    fib(3) should be (1)
    fib(4) should be (2)
    fib(5) should be (3)
    fib(6) should be (5)
  }

  "fib" should "produce expected value for large inputs" in {
    fib(30) should be (514229)

  }


  "fib" should "should fail for n below 1" in {
    intercept[IllegalArgumentException] {
      fib(0)
    }

    intercept[IllegalArgumentException] {
      fib(-1)
    }
  }

  // Exercise 2.2
  "isSorted" should "be true for empty string array" in {
    isSorted[String](Array(), (a, b) => a.compareTo(b) > 0 ) shouldBe true
  }

  "isSorted" should "be true for empty int array" in {
    isSorted[String](Array(), (a, b) => a.compareTo(b) > 0 ) shouldBe  true
  }

  "isSorted" should "be true for array of strings sorted in ascending order" in {
    isSorted[String](Array("1", "2", "3"), (a, b) => b.compareTo(a) > 0 ) shouldBe true
  }

  "isSorted" should "be true for array of strings sorted in descending order" in {
    isSorted[String](Array("3", "2", "1"), (a, b) => a.compareTo(b) > 0 ) shouldBe true
  }

  "isSorted" should "be false for array of strings not in any sorted order" in {
    isSorted[String](Array("3", "5", "1"), (a, b) => a.compareTo(b) > 0 ) shouldBe false
  }

  "isSorted" should "be true for array of ints sorted in ascending order" in {
    isSorted[Int](Array(1, 2, 3), (a, b) => a < b ) shouldBe true
  }

  "isSorted" should "be true for array of ints sorted in descending order" in {
    isSorted[Int](Array(4, 3, 2), (a, b) => a > b ) shouldBe true
  }

  "isSorted" should "be false for array of int not in any sorted order" in {
    isSorted[Int](Array(4, 3, 6, 9), (a, b) => a > b ) shouldBe false
  }
 //  // Exercise 2.3
 // def curry[A, B, C](f: (A, B) => C) : A => (B => C)
 "curry" should " a function with two arguments return a function that takes one argument and returns a function " in {
   val f :(Int, Int) => Int = (x, y) => x + y
   val cf = curry(f)
   cf(2)(3) shouldBe 5
 }

  "curry" should " a function with two arguments return a function that can be partially applied " in {
    val f :(Int, Int) => Int = (x, y) => x + y
    val pcf = curry(f)(2)
    pcf(3) shouldBe 5
  }


  // Exercise 2.4
 // def uncurry[A, B, C](f: A => B => C )
  "uncurry" should " a convert a curried function (f: A => B => C ) into two arguments function f:(A, B) => C " in {
    val f :(Int, Int) => Int = (x, y) => x + y
    val cf = curry(f)
    cf(2)(3) shouldBe 5
    uncurry(cf)(2, 3) shouldBe 5
  }

 // exercise 2.5 . Compose
  "compose" should " satisfy composition f o g i.e g(x) -> y , f(y) -> z then f(g(x)) -> z" in {
    val g : String => String = s => s.reverse
    val f : String => Int = s => s.toInt

    g("123") shouldBe "321"
    f("123") shouldBe 123

    compose(f, g)("123") shouldBe 321
    compose(f, g)("123") shouldBe f(g("123"))
  }




}
