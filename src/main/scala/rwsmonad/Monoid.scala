package rwsmonad

trait Monoid[A]:
  extension (x: A) def combine(y: A): A
  def empty: A

object Monoid extends Monoid2:
  given [A]: Monoid[List[A]] with
    extension (x: List[A])
      def combine(y: List[A]): List[A] =
        x ++ y
    def empty: List[A] = Nil

trait Monoid2:
  given Monoid[Int] with
    extension (x: Int) def combine(y: Int): Int = x + y
    def empty: Int = 0
