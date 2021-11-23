package rwsmonad

trait Applicative[F[_]] extends Functor[F]:
  extension [A, B](ff: F[A => B]) def ap(fa: F[A]): F[B]

  def pure[A](a: A): F[A]

  extension [A](fa: F[A])
    override def map[B](f: A => B): F[B] =
      pure(f).ap(fa)
