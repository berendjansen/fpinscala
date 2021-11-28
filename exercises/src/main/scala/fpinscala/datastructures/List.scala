package fpinscala.datastructures

import scala.collection.View.Fill

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("Empty list")
    case Cons(_, xs) => xs    
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => sys.error("Empty list")
    case Cons(_, xs) => Cons(h, xs)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, xs) => drop(xs, n-1)
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if f(x) => dropWhile(xs, f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("init of empty list")
    case Cons(_, Nil) => l
    case Cons(x, xs) => init(xs)
  }

  def init2[A](l: List[A]): List[A] = {
    var newL = List()
    @annotation.tailrec
    def go(cur: List[A]): List[A] = cur match {
      case Nil => sys.error("init of empty list")
      case Cons(_, Nil) => cur
      case Cons(h, t) => {
        append(newL, List(h))
        go(t)
      }
    }
    go(l)
  }

  def length[A](l: List[A]): Int = {
    foldRight(l, 0)((_,y) => 1 + y)
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    l match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z,x))(f)
      }
  }

  def sum3(ns: List[Int]) =
    foldLeft(ns, 0)(_ + _)

  def product3(ns: List[Double]) =
    foldLeft(ns, 1.0)(_ * _)

  def length3[A](ns: List[A]): Int = foldLeft(ns, 0)((x, h) => x + 1)

  def reverse[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => {
      foldLeft(l, List[A]())((acc, h) => Cons(h, acc))}
  }

  // 3.14 append in terms of foldleft or foldright
  def append2[A](a1: List[A], a2: List[A]): List[A] = {
    foldRight(a1, a2)((x, y) => Cons(x,y))
  }

  def append3[A](a1: List[A], a2: List[A]): List[A] = {
    foldLeft(reverse(a1), a2)((a, b) => Cons(b, a))
  }

  def concat[A](ls: List[List[A]]) : List[A] = {
    foldRight(ls, Nil:List[A])(append(_,_))
  }

  def add1(l: List[Int]): List[Int] = {
    foldRight(l, Nil: List[Int])((h, t) => Cons(h+1, t))
  }

  def stringToDouble(l: List[String]): List[Double] = {
    foldRight(l, Nil: List[Double])((h,t) => Cons(h.toDouble, t))
  }

  def map[A,B](l: List[A])(f: A => B): List[B] = {
    foldRight(l, Nil: List[B])((h,t) => Cons(f(h), t))
  }

  def map_2[A,B](l: List[A])(f: A => B): List[B] = {
    val tmpBuf = new collection.mutable.ListBuffer[B]
    def go(l: List[A]): Unit = l match{
      case Nil => Nil
      case Cons(h, t) => {
        tmpBuf += f(h)
        go(t)
      }
    }
    go(l)
    List(tmpBuf.toList: _*)
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    foldRight(as, Nil:List[A])((h, t) => if (f(h)) Cons(h, t) else t)
  }

  def filter2[A](as: List[A])(f: A => Boolean): List[A] = {
    val tmpBuf = new collection.mutable.ListBuffer[A]
    def go(l: List[A]): Unit = l match {
      case Nil => ()
      case Cons(h, t) => {
        if (f(h)) tmpBuf += h
        go(t)
      }
    }
    go(as)
    List(tmpBuf.toList: _*)
  }

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
    concat(map(l)(f))
  }

  def filterWithFlatMap(as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)( a => if (f(a)) List(a) else Nil)
  }

  @annotation.tailrec
  def addPairwise(l1: List[Int], l2: List[Int]): List[Int] = (a, b) match {
    case (_, Nil) => sys.error("empty list")
    case (Nil, _) => sys.error("empty list")      
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addPairwise(t1, t2))
  }

  @annotation.tailrec
  def zipWith[A,B,C](l1: List[A], l2: List[B])(f: (A,B) => C): List[C] = (a, b) match {
    case (_, Nil) => sys.error("empty list")
    case (Nil, _) => sys.error("empty list")      
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    def checkNow(l: List[A]): Boolean = {
      val ll = length(l)
      val ls = length(sub)
      val rl = reverse(l)
      if (reverse(drop(rl, ll-ls)) == sub) true else false
    }
    def go(l: List[A]): Boolean = l match {
      case Nil => false
      case Cons(h, t) => if (checkNow(l)) true else go(t)
    }
   go(sup)
  }
}

object Main {
  def main(args: Array[String]): Unit ={
    val l = List(1,2,3,4,5)
    val l2 = List(3, 2)
    println(List.flatMap(l)(i => List(i,i)))
    // println(List.drop(l, 3))
  }
}
