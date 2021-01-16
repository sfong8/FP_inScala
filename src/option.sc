trait Option[+A] {
  def map[B](f: A => B): Option[B]= this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def flatMap[B](f: A => Option[B]): Option[B] = {
    map(f).getOrElse(None)
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None =>  default
    case Some(a) => a
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case _ => this
  }


  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C]= {
    a.flatMap(aa => b map (bb => f(aa, bb)))
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a match {
      case Nil => Some(Nil)
      case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
    }
}


case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]



