package rwsmonad

opaque type State[S, A] = S => (S, A)

object State:
  extension [S, A](state: State[S, A]) def apply(s: S): (S, A) = state(s)

  def get[S]: State[S, S] =
    s => (s, s)

  def put[S](s: S): State[S, Unit] =
    _ => (s, ())

  def modify[S](f: S => S): State[S, Unit] =
    for {
      s <- get
      _ <- put(f(s))
    } yield ()

  given [S]: Functor[[A] =>> State[S, A]] with
    extension [A](fa: State[S, A])
      def map[B](f: A => B): State[S, B] =
        s =>
          val (s2, a) = fa(s)
          (s2, f(a))

  given [S]: Applicative[[A] =>> State[S, A]] with
    extension [A, B](ff: State[S, A => B])
      def ap(fa: State[S, A]): State[S, B] =
        s =>
          val (s2, f) = ff(s)
          val (s3, a) = fa(s2)
          (s3, f(a))

    def pure[A](a: A): State[S, A] =
      s => (s, a)

  given [S]: Monad[[A] =>> State[S, A]] with
    extension [A](fa: State[S, A])
      def flatMap[B](f: A => State[S, B]): State[S, B] =
        s =>
          val (s2, a) = fa(s)
          f(a)(s2)

    def pure[A](a: A): State[S, A] =
      s => (s, a)
