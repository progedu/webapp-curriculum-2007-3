trait StreamStudy[+A] {

  def headOption: Option[A] = ???

  def tail: StreamStudy[A] = ???

}

case object EmptyStream extends StreamStudy[Nothing]

case class Cons[+A](h: () => A, t: () => StreamStudy[A]) extends StreamStudy[A]

object StreamStudy {

  def cons[A](h: => A, t: => StreamStudy[A]): StreamStudy[A] = ???

  def empty[A]: StreamStudy[A] = EmptyStream

}
