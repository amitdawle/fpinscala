package ad

import org.scalatest._

import com.ad.Chapter2._

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


}
