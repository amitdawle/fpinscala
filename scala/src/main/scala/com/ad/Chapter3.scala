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


}
