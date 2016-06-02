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
    case Cons (h, t) =>  foldLeft(t)(f(z,h))(f)
  }

  //  -- Exercise 3.11
  def sum(l: List[Int]) : Int = {
    foldLeft(l)(0){(a,b) => a + b}
  }

  def product(l: List[Int]) : Int = {
    foldLeft(l)(1){(a,b) => a * b}
  }

  def length[A](l: List[A]) : Int = {
    foldLeft(l)(0){(a,b) => a + 1}
  }

  def reverse[A](l: List[A]) : List[A]= {
    foldLeft(l)(Nil:List[A]){(a,b) => Cons(b, a)}
  }


  def foldLeftWithFoldRight[A,B](l: List[A])(z: B)(f: (B, A) => B) : B = {
    foldRight(l){i:B => i}{(x:A, g) => {(t:B) => g(f(t , x))} }(z)
  }

  def append[A](to: List[A] , from:List[A]) : List[A] = {
    foldRight(to)(from){(x,t) => Cons(x, t)}
  }

  def concat[A](l: List[List[A]] ) : List[A] = {
    foldRight(l)(Nil:List[A]){(x,t) => append(x,t)}
  }

  // Exercise 3.16, 3.17 and 3.18
  def map[A,B](l : List[A])(f:A => B):List[B] = {
    foldRight(l)(Nil:List[B]){(x,t) => Cons(f(x) , t)}
  }


  // Exercise 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    foldRight(as)(Nil:List[A]){(x,t) => if( f(x) )Cons(x, t) else t}
  }


  // Exercise 3.20
  def flatMap[A](as: List[A])(f: A => List[A]): List[A] = {
    foldRight(as)(Nil:List[A]){(x,t) => append(f(x),t)}
  }

  // Exercise 3.21
  def filterwithFlatMap[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)((x) => if(f(x)) Cons(x,Nil) else Nil )
  }


  // Exercise 3.22, 3.23
  def zipWith[A,B,C](as:List[A], bs:List[B])(combine:(A,B)=>C) : List[C] = (as, bs) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(x,xs) , Cons(y, ys)) => Cons(combine(x,y) , zipWith(xs,ys)(combine) )
  }

  //Exercise 3.24
  def hasSubsequence[A](sup: scala.List[A], sub: scala.List[A]): Boolean = {
    sup.inits.flatMap(_.tails).filter(_.nonEmpty). contains(sub)
  }

  // Exercise 3.25-3.28

  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  def size[A](t:Tree[A]) : Int = t match {
    case Leaf(_) => 1
    case (Branch(l,r))  => 1 + size(l) + size(r)
  }

  def maximum(t:Tree[Int]) : Int = t match {
    case Leaf(v) => v
    case (Branch(l,r))  => maximum(l) max maximum(r)
  }

  def depth(t:Tree[Int]) : Int = t match {
    case Leaf(v) => 1
    case (Branch(l,r))  =>depth(l) max depth(r)
  }


  def fold[A,B](t:Tree[A])(f:A => B )(g:(B,B) => B) : B = t match {
    case Leaf(v) => f(v)
    case (Branch(l,r)) => g(fold(l)(f)(g) , fold(r)(f)(g))
  }

  def sizeWithFold[A](t:Tree[A]): Int = {
    fold(t)(_ => 1)(1 + _ + _)
  }

  def depthWithFold[A](t:Tree[A]): Int = {
    fold(t)(_ => 1 )( (l,r) => (l + 1) max (r + 1))
  }

  def mapWithFold[A,B](t:Tree[A])(f: A => B)(g:(B , B) => B) : B = {
    fold(t)(f)(g)
  }



}