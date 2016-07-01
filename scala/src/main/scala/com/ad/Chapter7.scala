package com.ad

import java.util.concurrent.{Callable, TimeUnit, Future, ExecutorService}


object Chapter7 {

  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  object Par {
    def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

    private case class UnitFuture[A](get: A) extends Future[A] {
      def isDone = true

      def get(timeout: Long, units: TimeUnit) = get

      def isCancelled = false

      def cancel(evenIfRunning: Boolean) = false
    }

    def fork[A](a: => Par[A]): Par[A] = {
      es => es.submit(new Callable[A] {
        override def call(): A = a(es).get
      })
    }

    def delay[A](a: => Par[A]): Par[A] = {
      //es => a(es)
          // or ???
      es => run(es)(a)
    }

    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))


    def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = {
      (e: ExecutorService) => UnitFuture(f(a(e).get, b(e).get))
    }


    def asyncF[A, B](f: A => B): A => Par[B] = {
      (a: A) => lazyUnit(f(a))
    }

    // Hard: Write this function, called sequence. No additional primitives are required. Do not call run.


    def sequenceWithRun[A](ps: List[Par[A]]): Par[List[A]] = {
      (es: ExecutorService) => {
        if (ps.isEmpty) UnitFuture(Nil)
        else UnitFuture(ps.head(es).get :: sequenceWithRun(ps.tail)(es).get)
      }
    }
 

    def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
      ps.foldRight(unit(Nil): Par[List[A]]) { (x: Par[A], t: Par[List[A]]) => Par.map2(x, t) { (a, b) => (a :: b) } }
    }


    def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = fork {
      val m = as.map {
        asyncF((a: A) => if (f(a)) a :: Nil else Nil)
      }
      map2(sequence(m), unit()) { (a, b) => a.flatten }
    }


    def sum(ints: Seq[Int]): Par[Int] = {
      if (ints.size <= 1) {
        unit(ints.headOption.getOrElse(0))
      } else {
        val (l, r) = ints.splitAt(ints.length / 2)
        Par.map2(fork(sum(l)), fork(sum(r)))((a, b) => a + b)
      }
    }


    def genPar[A](ints: Seq[A])(f:(A, A) => A)(d: A): Par[A] = {
      if (ints.size <= 1) {
        unit(ints.headOption.getOrElse(d))
      } else {
        val (l, r) = ints.splitAt(ints.length / 2)
        Par.map2(fork(genPar(l)(f)(d)), fork(genPar(r)(f)(d)))((a, b) => f(a,b))
      }
    }

    def sumWithGenPar(ints: Seq[Int]): Par[Int] = genPar(ints)((a,b)=> a + b)(0)

    def maxWithGenPar(ints: Seq[Int]): Par[Int] = genPar(ints)((a,b)=> if(a > b) a else b )(Int.MinValue)

    def parWordCount(p : List[String]) : Par[Int] = {
      if (p.size == 0){
        unit(0)
      } else if (p.size == 1){
        println(Thread.currentThread())
        unit(p.head.split(" ").length)
      }
      else {
        val (l, r) = p.splitAt(p.length / 2)
        Par.map2(fork(parWordCount(l)), fork(parWordCount(r)))((a, b) => a + b )
      }
    }

    def map[A,B](pa :Par[A]) (f:A => B) : Par[B] = map2(pa , unit())((a,_) => f(a))

    def map3[A, B, C, D](a: Par[A], b: Par[B], c:Par[C])(f: A => B => C => D): Par[D] = {
      map2( map2(a , b)((x,y) => f(x)(y)) , c)((g,z)=> g(z))
    }

    def map4[A, B, C, D, E](a: Par[A], b: Par[B], c:Par[C], d: Par[D])(f: A => B => C => D => E): Par[E] = {
      map2(map3(a, b, c)(w =>(x =>( y => f(w)(x)(y)))), d)( (g, z) => g(z))
    }

    def map5[A, B, C, D, E, F](a: Par[A], b: Par[B], c:Par[C], d: Par[D], e: Par[E])(f: A => B => C => D => E => F): Par[F] = {
      map3 (map3(a, b, c)(v =>(w =>(x => f(v)(w)(x)))), d, e)((g =>( y =>( z => g(y)(z)) ) ))
    }


     // 7.11
    def choiceN[A](n: Par[Int])(choices : List[Par[A]]): Par[A] = {
      es => choices( n(es).get )(es)
    }

    def choice[A](cond: Par[Boolean])(t: Par[A], f:Par[A]): Par[A] = {
      choiceN( map(cond)(x => if(x) 0 else 1 ) )(List(t,f))
    }

    //7.12
    def choiceMap[K,V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] = {
      es =>  choices(key(es).get)(es)
    }

    //7.13
    def chooser[A,B](pa: Par[A])(choices: A => Par[B]): Par[B] = {
      es =>  choices(pa(es).get)(es)
    }

    // 7.13
    def choiceNWithChooser[A](n: Par[Int])(choices : List[Par[A]]): Par[A] = {
      chooser(n)(i => choices(i))
    }

    // 7.13
    def choiceWithChooser[A](cond: Par[Boolean])(t: Par[A], f:Par[A]): Par[A] = {
      chooser(cond)(b => if(b) t else f )
    }

    def flatmap[A,B](pa: Par[A])(choices: A => Par[B]): Par[B] = chooser(pa)(choices)

    // 7.14
    def join[A](a : Par[Par[A]]) : Par[A] = {
      es:ExecutorService =>  (a(es).get())(es)
    }

    // 7.14a
    def flatmapWithJoin[A,B](pa: Par[A])(choices: A => Par[B]): Par[B] = {
      join(map(pa)(choices))
    }

    // 7.14b
    def joinwithFlatmap[A](a : Par[Par[A]]) : Par[A] = {
      flatmap(a)( identity )
    }

    def map2WithFlatMap[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] = {
      flatmap(pa)(a => flatmap(pb)( b => unit( f(a,b)) )   )
    }
  }
}

