package rwsmonad

trait CatsCompat:
  given [A: Monoid]: cats.kernel.Monoid[A] with
    def combine(x: A, y: A): A = x.combine(y)
    def empty: A = summon[Monoid[A]].empty

  given [F[_]: Functor]: cats.Functor[F] with
    def map[A, B](fa: F[A])(f: A => B): F[B] = fa.map(f)

  given [F[_]: Applicative]: cats.Applicative[F] with
    def ap[A, B](ff: F[A => B])(fa: F[A]): F[B] = ff.ap(fa)
    def pure[A](a: A): F[A] = summon[Applicative[F]].pure(a)

  given [F[_]: Monad]: cats.Monad[F] with cats.StackSafeMonad[F] with
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = fa.flatMap(f)
    def pure[A](a: A): F[A] = summon[Monad[F]].pure(a)
