import List.*
// Exercise 3.2

// Implement tail function

def tail[A](list: List[A]): List[A] = list match
  case Nil           => sys.error("List is nil")
  case (Cons(x, xs)) => xs

// Exercise 3.3

// Implement setHead for replacing the firs element in the list

def setHead[A](list: List[A], newValue: A): List[A] = list match
  case Nil         => sys.error("LIst is Nil")
  case Cons(x, xs) => Cons(newValue, xs)

// Exercise 3.4

// Implement the function drop which removes the first n elements from a list

def drop[A](list: List[A], n: Int): List[A] = list match
  case Nil => Nil
  case Cons(x, xs) =>
    n match
      case 0      => list
      case _: Int => drop(xs, n - 1)

// Exercise 3.5

// Implement dropWhile which removes elements from List prefix as long as they match the predicate

def dropWhile[A](list: List[A], f: A => Boolean): List[A] = list match
  case Nil => Nil
  case Cons(x, xs) =>
    if f(x) then dropWhile(xs, f)
    else list

// Exercise 3.6

// Implement init that returns a List consisting of all but the last element of a List

def init[A](list: List[A]): List[A] = list match
  case Nil              => sys.error("List is nil")
  case Cons(head, Nil)  => Nil
  case Cons(head, tail) => Cons(head, init(tail))

// Exercise 3.7

// Terminate foldRight

// def foldRight[A, B](
//     list: List[A],
//     acc: B,
//     f: (A, B) => B,
//     terminatePredicate: (A) => Boolean,
//     terminateValue: B
// ): B = list match
//   case Nil => acc
//   case Cons(head, tail) =>
//     if terminatePredicate(head) then terminateValue
//     else f(head, foldRight(tail, acc, f, terminatePredicate, terminateValue))

def foldRight[A, B](
    list: List[A],
    acc: B,
    f: (A, B) => B
): B = list match
  case Nil              => acc
  case Cons(head, tail) => f(head, foldRight(tail, acc, f))

// Exercise 3.9

// Compute the length of a list using foldRight

def length[A](list: List[A]): Int = foldRight(list, 0, (head, rest) => 1 + rest)

// Implement sorting using foldRight

// def head[A](a: List[A]) = a match
//   case Nil         => sys.error("error")
//   case Cons(h, tl) => h

// def ins(el: Int, list: List[Int]): List[Int] = list match
//   case Nil => Cons(el, list)
//   case _ =>
//     val hd = head(list)
//     val tl = tail(list)
//     if el <= hd then Cons(el, list) else Cons(hd, ins(el, tl))

// def srt(list: List[Int]) = foldRight(list, List(0), ins)

// Exercise 3.10

// Imeplement foldLeft

def foldLeft[A, B](as: List[A], acc: B, f: (B, A) => B): B =
  def loop(currentList: List[A], result: B): B = currentList match
    case Nil              => result
    case Cons(head, tail) => loop(tail, f(result, head))

  loop(as, acc)

// Exercise 3.11

// Implement sum, product, a length using foldLeft

def sum(list: List[Int]): Int = foldLeft(list, 0, _ + _)
def product(list: List[Int]): Int = foldLeft(list, 1, _ * _)
def lengthLeft[A](list: List[A]): Int =
  foldLeft(list, 0, (res, head) => 1 + res)

// Exercise 3.12

// Implement reverse function using fold

def reverseFold[A](list: List[A]): List[A] =
  foldLeft(list, Nil, (result: List[A], head: A) => Cons(head, result))
