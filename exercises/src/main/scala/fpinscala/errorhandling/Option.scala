package fpinscala.errorhandling


import scala.annotation.tailrec
import scala.{Either => _, Option => _, Some => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(x) => Some(f(x))
  }

  def getOrElse[B>:A](default: => B): B = this match {
    case Some(x) => x
    case _ => default
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }

  def orElse[B>:A](ob: => Option[B]): Option[B] = this match {
    case Some(_) => this
    case _ => ob
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => this
    case _ => None
  }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  def variance(xs: Seq[Double]): Option[Double] = ???

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a.flatMap(x => b.map(y => f(x, y)))

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    @tailrec
    def sequence(as: List[Option[A]], l: List[A]): Option[List[A]] = as match {
      case Nil => Some(l.reverse)
      case None::_ => None
      case Some(a)::as => sequence(as, a::l)
    }
    sequence(a, Nil)
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    @tailrec
    def traverse(as: List[A], bs: List[B])(f: A => Option[B]): Option[List[B]] = as match {
      case Nil => Some(bs.reverse)
      case a::as =>
          f(a) match {
          case Some(b) => traverse(as, b::bs)(f)
          case _ => None
        }
    }

    traverse(a, Nil)(f)
  }

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch {case _: Exception => None}
}