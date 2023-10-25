import List.*
import Tree.*
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

// Exercise 3.13

// Write foldRight in terms of foldLeft
def foldRightL[A, B](list: List[A], acc: B, f: (B, A) => B): B =
  foldLeft(reverseFold(list), acc, f)

// Exercise 3.14

// Write append in terms of fold

def appendFold[A](as: List[A], bs: List[A]): List[A] =
  foldRight(as, bs, (head, result) => Cons(head, result))

// Exercise 3.15

// Write a function that concatenates a list of lists into a single list

def concat[A](list: List[List[A]]): List[A] =
  foldRight(
    list,
    Nil,
    (head: List[A], result: List[A]) => appendFold(head, result)
  )

// Exercise 3.16

// Write a function that transforms a list of integers by adding 1 to each element

def incList(list: List[Int]): List[Int] = list match
  case Nil              => Nil
  case Cons(head, tail) => Cons(head + 1, incList(tail))

// Exercise 3.17

// Write a function that turns each value in a List[Double] into a String.

def doublesToStrings(list: List[Double]): List[String] = list match
  case Nil              => Nil
  case Cons(head, tail) => Cons(head.toString(), doublesToStrings(tail))

// Exercise 3.18

// Write map

def map[A, B](list: List[A], f: (A) => B): List[B] = list match
  case Nil              => Nil
  case Cons(head, tail) => Cons(f(head), map(tail, f))

// Exercise 3.19

// Write filter

def filter[A](list: List[A], f: (A) => Boolean): List[A] = list match
  case Nil => Nil
  case Cons(head, tail) =>
    if f(head) then Cons(head, filter(tail, f)) else filter(tail, f)

// Exercise 3.20

// Write flatMap

def flatMap[A, B](list: List[A], f: A => List[B]): List[B] = concat(
  map(list, f)
)

// Exercise 3.21

// Use flatMap to implement filter

def filterFlatmap[A](list: List[A], f: A => Boolean): List[A] =
  flatMap(list, (a: A) => if f(a) then List(a) else Nil)

// Exercise 3.22

// Add elements of two lists

def addTwoLists(a: List[Int], b: List[Int]): List[Int] = a match
  case Nil =>
    b match
      case Nil         => Nil
      case Cons(h, tl) => sys.error("Lists have different size")

  case Cons(head, tail) =>
    b match
      case Nil                => sys.error("Lists have different size")
      case Cons(head2, tail2) => Cons(head + head2, addTwoLists(tail, tail2))

// Exercise 3.23

// Generalize the previous function

def combineLists[A, B](a: List[A], b: List[A], f: (A, A) => B): List[B] =
  a match
    case Nil =>
      b match
        case Nil         => Nil
        case Cons(h, tl) => sys.error("Lists have different size")

    case Cons(head, tail) =>
      b match
        case Nil => sys.error("Lists have different size")
        case Cons(head2, tail2) =>
          Cons(f(head, head2), combineLists(tail, tail2, f))

// Exercise 3.24

// Implement hasSubSequence
def subFromStart[A](list: List[A], sub: List[A]): Boolean = sub match
  case Nil => true
  case Cons(head, tail) =>
    list match
      case Nil => false
      case Cons(head2, tail2) =>
        if head == head2 then subFromStart(tail2, tail) else false

def hasSubSequence[A](list: List[A], sub: List[A]): Boolean = sub match
  case Nil => true
  case _ =>
    list match
      case Nil => false
      case Cons(_, tail) =>
        subFromStart(list, sub) || hasSubSequence(tail, sub)

// Exercise 3.25

// Write a function, maximum, that returns the maximum element in a Tree[Int]

def maximum(tree: Tree[Int]): Int = tree match
  case Leaf(value)         => value
  case Branch(left, right) => maximum(left).max(maximum(right))

// Exercise 3.26

// Write a function, depth, that returns the maximum path length from the root of a tree to any leaf

def depth[A](tree: Tree[A]): Int = tree match
  case Leaf(value)         => 1
  case Branch(left, right) => (1 + depth(left)).max(1 + depth(right))

// Exercise 3.27

// Write map for Tree

def mapTree[A, B](tree: Tree[A], f: A => B): Tree[B] = tree match
  case Leaf(value)         => Leaf(f(value))
  case Branch(left, right) => Branch(mapTree(left, f), mapTree(right, f))

// Exercise 3.28

// Write fold for Tree

def foldTree[A, B](tree: Tree[A], f: (B, B) => B, acc: A => B): B = tree match
  case Leaf(value)         => acc(value)
  case Branch(left, right) => f(foldTree(left, f, acc), foldTree(right, f, acc))

def sizeFold[A](tree: Tree[A]): Int = foldTree(
  tree,
  (leftResutl, rightResult) => (1 + leftResutl).max(1 + rightResult),
  (_) => 1
)

def maxFold(tree: Tree[Int]): Int = foldTree(
  tree,
  (leftResult, rightResult) => leftResult.max(rightResult),
  value => value
)

def mapFold[A, B](tree: Tree[A], f: A => B): Tree[B] = foldTree(
  tree,
  (leftResult, rightResult) => Branch(leftResult, rightResult),
  (value) => Leaf(f(value))
)
