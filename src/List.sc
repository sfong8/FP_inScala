
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]


object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def tail[A](listx:List[A]) : List[A] = listx match {
    case Nil => Nil
    case Cons(x,xs) => xs
  }

  def setHead[A](listx: List[A],firstValue: A): List[A] = listx match {
    case Nil => Nil
    case Cons(x,xs) => Cons(head = firstValue,tail = xs)
  }
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n==0)
      l
    else
      drop[A](List.tail(l),n-1)
  }
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(h,t) if f(h) => dropWhile(t, f)
      case _ => l
    }
  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => sys.error("init of empty list")
      case Cons(_,Nil) => Nil
      case Cons(h,t) => Cons(h,init(t))
    }
  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)
  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _)

  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((x,y)=> y+1)
  }
  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r)(Cons(_,_))

  def addOne(listx : List[Int]) : List[Int] = {
    foldRight(listx,Nil:List[Int])((head,tail)=> Cons(head+1,tail))
  }
  def to_string(listx : List[Double]) : List[String] = {
    foldRight(listx,Nil:List[String])((head,tail)=> Cons(head.toString,tail))
  }

  def map[A,B](as: List[A])(f: A => B): List[B] = {
    f(, foldRight(xs, z)(f))
  }
}

val x = List(1,2,3,4,5) match {
  case Cons(x, Cons(2, Cons(4, _))) => x
  case Nil => 42
  case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y case Cons(h, t) => h + List.sum(t)
  case _ => 101


}
val y =List(1,2,3,4,5)
val z = List.init(y)
val z2 = List.addOne(y)
val xs: List[Int] = List(1,2,3,4,5)
val ex1 = List.dropWhile(xs, (x: Int) => x < 4)



val y2 =List(1.2,2.5,3.4,4.5,5.6)
val z3 =List.to_string(y2)
