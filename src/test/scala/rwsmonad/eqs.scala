package rwsmonad

import cats.kernel.Eq

object StateEq:
  given [S: Monoid: Eq, A: Eq]: Eq[State[S, A]] =
    Eq.by[State[S, A], (S, A)](_(summon[Monoid[S]].empty))

object ReaderEq:
  given [R: Monoid, A: Eq]: Eq[Reader[R, A]] =
    Eq.by[Reader[R, A], A](_(summon[Monoid[R]].empty))
