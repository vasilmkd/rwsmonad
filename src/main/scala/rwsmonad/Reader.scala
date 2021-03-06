package rwsmonad

opaque type Reader[R, A] = R => A

object Reader:
  extension [R, A](ra: Reader[R, A])
    def apply(r: R): A = ra(r)

    def local[R1](f: R1 => R): Reader[R1, A] =
      r1 => ra(f(r1))

  def ask[R]: Reader[R, R] = identity

  def asks[R, A](f: R => A): Reader[R, A] = f

  given [R]: Functor[[A] =>> Reader[R, A]] with
    extension [A](fa: Reader[R, A])
      def map[B](f: A => B): Reader[R, B] =
        r => f(fa(r))

  given [R]: Applicative[[A] =>> Reader[R, A]] with
    extension [A, B](ff: Reader[R, A => B])
      def ap(fa: Reader[R, A]): Reader[R, B] =
        r =>
          val f = ff(r)
          val a = fa(r)
          f(a)

    def pure[A](a: A): Reader[R, A] = _ => a

  given [R]: Monad[[A] =>> Reader[R, A]] with
    extension [A](fa: Reader[R, A])
      def flatMap[B](f: A => Reader[R, B]): Reader[R, B] =
        r =>
          val a = fa(r)
          f(a)(r)

    def pure[A](a: A): Reader[R, A] = _ => a
