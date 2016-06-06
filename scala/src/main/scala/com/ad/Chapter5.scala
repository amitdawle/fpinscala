package com.ad


object Chapter5 {

  sealed trait Stream[+A] {

    def headOption : Option[A] = this match {
      case Empty => None
      case Cons(h, t) => Some( h())
    }

    // 5.1
    def toList : List[A] = this match {
      case Empty => Nil
      case Cons(h, t) => h() :: t().toList
    }
    // 5.2
    def take(n:Int) : Stream[A] = this match {
      case Empty => Stream.empty
      case Cons(h, t) if n > 0 => Stream.cons(h() , t().take(n-1))
      case _ =>  Stream.empty
    }

    def drop(n:Int) : Stream[A] = this match {
      case Empty => Stream.empty
      case Cons(h, t) if n > 0 =>  t().drop(n-1)
      case _ =>  this
    }

    //5.3
    def takeWhile(p: A => Boolean) : Stream[A] = this match {
      case Empty => Stream.empty
      case Cons(h, t) if p(h()) => Stream.cons(h() , t().takeWhile(p))
      case _ =>  Stream.empty
    }

    def foldRight[B](z: => B)(f:(A , =>B) => B) : B = this match {
      case Empty => z
      case Cons(h, t) => f(h() , t().foldRight(z)(f))
    }
    // 5.4
    def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b )

    // 5.5
    def takeWhileWithFoldRight(p: A => Boolean): Stream[A] =
      foldRight(Stream.empty:Stream[A])((a,b) => if( p(a) ) Stream.cons(a, b) else Stream.empty )

    // 5.6
    def headOptionWithFoldRight : Option[A] = foldRight(None:Option[A])((a, b) => Some(a) )

    //5.7
    //Exercise 5.7
    // Implement map, filter, append, and flatMap using foldRight. The append method should be non-strict in its argument.
    def map[B](f:A => B): Stream[B] = foldRight(Stream.empty:Stream[B])((a, b) => Stream.cons(f(a) , b) )

    def filter(p:A => Boolean): Stream[A] = foldRight(Stream.empty:Stream[A])((a, b) => if(p(a)) Stream.cons(a , b) else b )

    def append[B >: A ](xs:Stream[B] ): Stream[B] = foldRight(xs)((a, b) => Stream.cons(a , b) )

    def flatMap[B >: A](f:A => Stream[B]): Stream[B] = foldRight(Stream.empty:Stream[B])((a, b) => f(a).append(b) )

    //Exercise 5.13
    def mapWithUnfold[B](f:A => B): Stream[B]  = Stream.unfold(this)(s => if(s == Empty) None else Some(f(s.headOption.get), s.drop(1)))

    def takeWithUnfold(n:Int): Stream[A]  = if ( n < 0) Stream.empty else Stream.unfold((n,this))( s => if(s._1 == 0 || s._2 == Empty ) None else Some(s._2.headOption.get , (s._1 - 1, s._2.drop(1))))

    def takewhileWithUnfold(p: A => Boolean): Stream[A]  = Stream.unfold(this)(s => if( s == Empty || !p(s.headOption.get) ) None else Some(s.headOption.get , s.drop(1)))

    def zipWith[B](s2: Stream[B])(f:(A, =>B) => B) =
      Stream.unfold((this,s2)){ s =>
        if ( s._1 == Empty || s._2 == Empty) None else
          Some ( f(s._1.headOption.get , s._2.headOption.get ) , (s._1.drop(1) , s._2.drop(1)))  }

    def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
      Stream.unfold(this,s2){ s =>
        if ( s._1 == Empty && s._2 == Empty) None
        else Some( (s._1.headOption , s._2.headOption ) , (s._1.drop(1) , s._2.drop(1)) )
      }

    // 5.14
    def startsWith[B >: A ](s: Stream[B]): Boolean = this.zipAll(s).forAll(p => p._1 == p._2 || p._2.isEmpty)

    // 5.15
    def tails: Stream[Stream[A]] = Stream.unfold(this)(s => if(s == Empty) None else Some(s , s.drop(1)))

    def scanRight[B](z:B)(f: (A, => B) => B) : Stream[B]
    = Stream.unfold(this)(s => if(s == Empty) None else Some(s.foldRight(z)(f) , s.drop(1)))

    // 5.16
    def tailsWithScanRight: Stream[Stream[A]] = scanRight (Stream.empty:Stream[A])((a,b) => Stream.cons(a, b) )


  }


  case object Empty extends Stream[Nothing]
  case class Cons[+A](h : () => A , t : () => Stream[A]) extends Stream[A]


  object Stream {
    def cons[A](hd : => A, tl :  => Stream[A]) : Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head , () => tail)
    }

    def empty[A] : Stream[A] = Empty

    def apply[A]( as : A*) : Stream[A] = {
      if(as.isEmpty) empty else cons( as.head , apply(as.tail : _*) )
    }

    //5.8
    def constant[A](a: A) : Stream[A] = Stream.cons (a , constant(a))

    //5.9
    def from(n: Int): Stream[Int] = cons( n , from(n+1))

    //5.10
    def fib :Stream[Int] = {
      def go(a:Int , b:Int) : Stream[Int] = cons (a , go( b , a + b ))
      go( 0 , 1)
    }

    //5.11
    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
      f(z) match {
        case None => Stream.empty
        case Some((a,s)) => cons(a, unfold(s)(f))
      }
    }

    //5.12 - See tests

  }

}