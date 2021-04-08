package rwsmonad

opaque type Writer[L, A] = () => (L, A)

object Writer:
  extension [L, A] (wa: Writer[L, A])
    def apply(): (L, A) = wa()

    def censor(f: L => L): Writer[L, A] =
      () =>
        val (log, a) = wa()
        (f(log), a)

    def listen: Writer[L, (A, L)] =
      () =>
        val (log, a) = wa()
        (log, (a, log))

  def tell[L](log: L): Writer[L, Unit] = () => (log, ())

  given [L]: Functor[[A] =>> Writer[L, A]] with
    extension [A] (fa: Writer[L, A]) def map[B](f: A => B): Writer[L, B] =
      () =>
        val (log, a) = fa()
        (log, f(a))

  given [L: Monoid]: Applicative[[A] =>> Writer[L, A]] with
    extension [A, B] (ff: Writer[L, A => B]) def ap(fa: Writer[L, A]): Writer[L, B] =
      () =>
        val (logFF, f) = ff()
        val (logFA, a) = fa()
        (logFF.combine(logFA), f(a))

    def pure[A](a: A): Writer[L, A] =
      () => (summon[Monoid[L]].empty, a)

  given [L: Monoid]: Monad[[A] =>> Writer[L, A]] with
    extension [A] (fa: Writer[L, A]) def flatMap[B](f: A => Writer[L, B]): Writer[L, B] =
      () =>
        val (logFA, a) = fa()
        val (logFB, b) = f(a)()
        (logFA.combine(logFB), b)

    def pure[A](a: A): Writer[L, A] =
      () => (summon[Monoid[L]].empty, a)
