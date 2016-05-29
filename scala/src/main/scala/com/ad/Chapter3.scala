package com.ad

object Chapter3 {
  sealed trait List[+A]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]
  case object Nil extends List[Nothing]

  //3.2
  def tail[A](l: List[A] ): List[A] = l match {
    case Nil => throw new IllegalArgumentException("Cannot call tail on an empty list.")
    case Cons(h , t) => t
  }

  //3.3
  def setHead[A](a: A , l: List[A]) : List[A] = l match {
    case Nil => Cons(a , Nil)
    case Cons(h, t) => Cons(a, t)
  }
  //3.4
  def drop[A](l: List[A], n:Int ) : List[A] = (n, l) match {
    case (_, Nil) => Nil
    case (0,  Cons(h , t)) => Cons(h , t)
    case (i,  Cons(h , t)) => drop(t, i - 1)
  }

  //3.4
  def tailWithDrop[A](l: List[A]) : List[A] = l match {
    case Nil => throw new IllegalArgumentException ("tail on an empty list")
    case xs => drop(xs , 1)
  }

  //3.5
  def dropWhile[A](l: List[A], f:A => Boolean ) : List[A] = l match {
    case Nil => Nil
    case Cons(h, t) if f(h) => dropWhile(t , f)
    case ls => ls
  }


  // 3.6
  def init[A](l: List[A]) : List[A] = l match {
    case Nil => Nil
    case Cons(h, Nil) => Nil
    case Cons(h, t) => Cons(h , init(t))
  }


  def takeWhile[A](l: List[A]) (f: A => Boolean) : List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => if( f(h) ) Cons( h , takeWhile(t)(f)) else Nil
  }

  //  -- Exercise 3.7
  def foldRight[A, B](l: List[A])(z: B)(f:(A, B) => B): B = (l, z) match {
    case (Nil, b) => b
    case (Cons(h, t), b) => f (h, foldRight(t)(b)(f) )
  }


  // Exercise 3.8 is already covered by 3.7


  // Exercise 3.9
  def lengthWithFoldRight[A](l : List[A]) : Int = {
    foldRight(l)(0){(x, t:Int) => 1 + t}
  }

  // Exercise 3.10
  def foldLeft[A,B](l: List[A])(z: B)(f: (B, A) => B) : B = l match {
    case Nil => z
    case Cons (h, t) =>  foldLeft(l)(f(z,h))(f)
  }


  //
//  -- Exercise 3.11
//  sum :: (Num a) => (List a) -> a
//  sum = foldLeft (\t x -> x + t) 0
//
//  -- Exercise 3.11
//  product :: (Num a) => (List a) -> a
//  product = foldLeft (\t x -> x * t) 1
//
//
//  -- Exercise 3.11
//  length2 :: (Num b) => (List a) -> b
//  length2 = foldLeft (\t x -> t + 1) 0
//
//
//  -- Exercise 3.12
//  reverse :: (List a) -> (List a)
//  reverse = foldLeft (\t x -> Cons x t ) (Empty)
//
//
//  -- Exercise 3.13
//  foldLeftWithFoldRight :: (b -> a -> b) -> b -> (List a) -> b
//  foldLeftWithFoldRight f b l = foldRight (\x g t -> g ( f t x ) ) id l b
//


}


