trait StreamStudy[+A] {
  private[this] var result: Option[A] = None

  def headOption: Option[A] = this match {
    case Cons(h, _) => result match {
      case Some(_) => result
      case None => {
        val r = h()
        result = Some(r)
        Some(r)
      }
    }
    case EmptyStream => None
  }

  def tail: StreamStudy[A] = this match {
    case Cons(_, t) => t()
    case EmptyStream => throw new UnsupportedOperationException("tail of empty stream")
  }

}

case object EmptyStream extends StreamStudy[Nothing]

case class Cons[+A](h: () => A, t: () => StreamStudy[A]) extends StreamStudy[A]

object StreamStudy {

  def cons[A](h: => A, t: => StreamStudy[A]): StreamStudy[A] = Cons(() => h, () => t)

  def empty[A]: StreamStudy[A] = EmptyStream

}

// ↓↓↓↓↓ 正解のコード ↓↓↓↓↓
//trait StreamStudy[+A] {
//
//  def headOption: Option[A] = this match {
//    case EmptyStream => None
//    case Cons(h, t) => Some(h())
//  }
//
//  def tail: StreamStudy[A] = this match {
//    case EmptyStream => throw new NoSuchMethodError()
//    case Cons(h, t) => t()
//  }
//
//}
//
//case object EmptyStream extends StreamStudy[Nothing]
//
//case class Cons[+A](h: () => A, t: () => StreamStudy[A]) extends StreamStudy[A]
//
//object StreamStudy {
//
//  def cons[A](h: => A, t: => StreamStudy[A]): StreamStudy[A] = {
//    lazy val headResult = h
//    lazy val tailResult = t
//    Cons(() => headResult, () => tailResult)
//  }
//
//  def empty[A]: StreamStudy[A] = EmptyStream
//
//}