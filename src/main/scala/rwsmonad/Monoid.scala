package rwsmonad

trait Monoid[A]:
  extension (x: A) def combine(y: A): A
  def empty: A

object Monoid:
  given [A]: Monoid[List[A]] with
    extension (x: List[A]) def combine(y: List[A]): List[A] =
      x ++ y
    def empty: List[A] = Nil
