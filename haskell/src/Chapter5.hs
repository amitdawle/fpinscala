module Chapter5 where
import Prelude hiding (forall)

-- Haskell is lazy by default and hence some exercises are omitted

-- 5.4
forAll :: (a->Bool) -> [a] -> Bool -- forall is a keyword
forAll f  = foldr(\x t -> (f x) && t)(True)

-- 5.5 to 5.10 are simple hence omitted
-- 5.11
unfold :: (s -> Maybe(a,s)) -> s -> [a]
unfold f x = case (f x) of
              Nothing -> []
              Just(a, s) -> a : (unfold f s)


zipAll :: [a] -> [b] -> [(Maybe a , Maybe b)]
zipAll [] [] = []
zipAll (x:xs) [] = (Just x, Nothing) : ( zipAll xs [] )
zipAll [] (y:ys) = (Nothing, Just y) : ( zipAll [] ys)
zipAll (x:xs) (y:ys) = (Just x, Just y) : ( zipAll xs ys)

-- 5.14
startsWith :: (Eq a) => [a] -> [a] -> Bool
startsWith xs ys = forAll (\(x,y) -> x == y || y == Nothing ) (zipAll xs ys)

tails :: [a] -> [[a]]
tails = unfold (\xs -> if( null xs) then Nothing else Just(xs , tail xs))


scanRight :: (a -> b -> b) -> b -> [a] -> [b]
scanRight f b  = unfold (\xs -> if(null xs) then Nothing else Just(foldr f b xs , tail xs ))
{--

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

--}

