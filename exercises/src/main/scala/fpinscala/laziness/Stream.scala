package fpinscala.laziness

import Stream._

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) =>
      p(a) || b
    ) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] =
    this match {
      case Empty      => None
      case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
    }

  def toList(): List[A] = {
    @tailrec
    def toList(a: A, t: Stream[A], l: List[A]): List[A] =
      t match {
        case Empty      => a :: l
        case Cons(h, t) => toList(h(), t(), a :: l)
      }

    this match {
      case Empty      => Nil
      case Cons(h, t) => toList(h(), t(), Nil).reverse
    }
  }

  def toListBuffered(): List[A] = {
    val b = ListBuffer[A]()
    @tailrec
    def toList(s: Stream[A]): List[A] =
      s match {
        case Empty => b.toList
        case Cons(h, t) =>
          b += h()
          toList(t())
      }

    toList(this)
  }

  def take(n: Int): Stream[A] = {
    def take(s: Stream[A], n: Int): Stream[A] =
      (s, n) match {
        case (Empty, _) | (_, 0) => empty
        case (Cons(h, t), n)     => cons(h(), t().take(n - 1))
      }

    take(this, n)
  }

  def drop(n: Int): Stream[A] = {
    @tailrec
    def drop(s: Stream[A], n: Int): Stream[A] =
      s match {
        case Cons(_, t) if n > 0 => drop(t(), n - 1)
        case s if n == 0         => s
        case _                   => Empty
      }
    drop(this, n)
  }

  def takeWhile(p: A => Boolean): Stream[A] = {
    def takeWhile(s: Stream[A]): Stream[A] =
      s match {
        case Cons(h, t) if p(h()) => cons(h(), takeWhile(t()))
        case _                    => empty
      }

    takeWhile(this)
  }

  def forAll(p: A => Boolean): Boolean = this match {
    case Empty => true
    case Cons(h, t) if p(h()) => t().forAll(p)
    case _ => false
  }

  def takeWhileFold(p: A => Boolean): Stream[A] =
      foldRight(empty[A])((a, acc) => {
        if (p(a)) cons(a, acc)
        else empty
      })

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  def headOptionFold: Option[A] = foldRight(None: Option[A])((a, _) => Some(a))

  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((a, acc) => cons(f(a), acc))

  def filter(p: A => Boolean): Stream[A] = foldRight(empty[A])((a, acc) => if (p(a)) cons(a, acc) else acc)

  def append[B >: A](s: => Stream[B]): Stream[B] = foldRight(s)((a, ss) => cons(a, ss))

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((a, s) => f(a).append(s))
  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def startsWith[B](s: Stream[B]): Boolean = ???
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => Stream.empty
    case Some((a, s)) => Stream.cons(a, unfold(s)(f))
  }
}
