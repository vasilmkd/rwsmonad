package rwsmonad

trait Monad[F[_]] extends Applicative[F]:
  extension [A](fa: F[A]) def flatMap[B](f: A => F[B]): F[B]

  extension [A, B](ff: F[A => B])
    override def ap(fa: F[A]): F[B] =
      ff.flatMap(f => fa.flatMap(a => pure(f(a))))
