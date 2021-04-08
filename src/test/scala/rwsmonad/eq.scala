package rwsmonad

import cats.kernel.Eq

object StateEq:
  given [S: Monoid: Eq, A: Eq]: Eq[State[S, A]] =
    Eq.by[State[S, A], (S, A)] { s =>
      s(summon[Monoid[S]].empty)
    }
