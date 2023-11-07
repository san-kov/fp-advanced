import List.*
import Chapter3.*
enum Option[+A]:
  case Some(get: A)
  case None

  def map[B](f: A => B): Option[B] = this match
    case Some(get) => Some(f(get))
    case None      => None

  def flatMap[B](f: A => Option[B]): Option[B] = this match
    case Some(get) => f(get)
    case None      => None

  def getOrElse[B >: A](default: => B): B = this match
    case Some(get) => get
    case None      => default

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match
    case Some(get) => Some(get)
    case None      => ob

  def filter(f: A => Boolean): Option[A] = this match
    case Some(get) => if f(get) then Some(get) else None
    case None      => None

  def mean(xs: Seq[Double]): Option[Double] =
    if xs.isEmpty then None
    else Some(xs.sum / xs.length)

    // Exercise 4.2
    // Implement variance in terms of flatMap

    // def variance(xs: Seq[Double]): Option[Double] =
    //   mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

    //   def lift[A, B](f: A => B): Option[A] => Option[B] = _.map(f)

    //   val absO: Option[Double] => Option[Double] = lift(math.abs)

    // def toIntOption(s: String): Option[Int] =
    //   try Some(s.toInt)
    //   catch case _: NumberFormatException => None

    //   def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double =
    //     age.toDouble

    //     def parseInsuranceRateQuote(
    //         age: String,
    //         numberOfSpeedingTickets: String
    //     ): Option[Double] =
    //       val optAge: Option[Int] = toIntOption(age)
    //       val optTickets: Option[Int] = toIntOption(numberOfSpeedingTickets)
    //       map2(optAge, optTickets)(insuranceRateQuote)
import Option.*
// Exercise 4.3
// Write a generic function map2 that combines two Option values using a binary function. If either Option value is None, then the return value is too.
def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
  a match
    case None => None
    case Some(aVal) =>
      b match
        case None       => None
        case Some(bVal) => Some(f(aVal, bVal))

// Exercise 4.4
// Write a function sequence that combines a list of Options into one Option containing
// a list of all the Some values in the original list.

def sequence[A](as: List[Option[A]]): Option[List[A]] = Chapter3.foldRight(
  as,
  Some(Nil: List[A]),
  (head, result) => {
    head match
      case Some(get) =>
        result match
          case Some(restValues) => Some(Cons(get, restValues))
          case None             => None
      case None => None
  }
)

def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = as match
  case Nil => Some(Nil)
  case Cons(head, tail) =>
    val mapped = f(head)
    mapped match
      case None => None
      case Some(get) =>
        val restResult = traverse(tail)(f)
        restResult match
          case None             => None
          case Some(restMapped) => Some(Cons(get, restMapped))
