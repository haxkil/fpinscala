package fpinscala.errorhandling


import scala.annotation.tailrec
import scala.{Either => _, Left => _, Option => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E,+A] {
 def map[B](f: A => B): Either[E, B] = this match {
   case l: Left[E] => l
   case r: Right[A] => Right(f(r.get))
 }

 def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
   case l: Left[EE] => l
   case r: Right[A] => f(r.get)
 }


  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => b
    case r => r
  }

 def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = this match {
   case l: Left[E] => l
   case r: Right[A] => b match {
     case bl: Left[EE] => bl
     case br: Right[B] => Right(f(r.get, br.get))
   }
 }
}
case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either {
  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    @tailrec
    def traverse(as: List[A], bs: List[B])(f: A => Either[E, B]): Either[E, List[B]] = as match {
      case Nil => Right(bs.reverse)
      case a::as =>
        f(a) match {
          case l: Left[E] => l
          case r: Right[B] => traverse(as, r.get::bs)(f)
        }
    }
    traverse(es, Nil)(f)
  }

  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] = traverse(es)(x => x)

  def mean(xs: IndexedSeq[Double]): Either[String, Double] = 
    if (xs.isEmpty) 
      Left("mean of empty list!")
    else 
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] = 
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

}