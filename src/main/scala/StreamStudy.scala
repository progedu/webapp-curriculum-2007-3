trait StreamStudy[+A] {

  def headOption: Option[A] = this match {
    case EmptyStream => None
    case Cons(h, t) => Some(h())
  }

  def tail: StreamStudy[A] = this match {
    case EmptyStream => throw new NoSuchElementException
    case Cons(h, t) => t()
  }

}

case object EmptyStream extends StreamStudy[Nothing]

case class Cons[+A](h: () => A, t: () => StreamStudy[A]) extends StreamStudy[A]

object StreamStudy {

  def cons[A](h: => A, t: => StreamStudy[A]): StreamStudy[A] = {
    lazy val lazyHead = h
    lazy val lazyTail = t
    Cons(() => lazyHead, () => lazyTail)
  }

  def empty[A]: StreamStudy[A] = EmptyStream

}
