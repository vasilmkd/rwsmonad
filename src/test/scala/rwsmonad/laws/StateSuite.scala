package rwsmonad
package laws

import cats.laws.discipline.*
import munit.DisciplineSuite

class StateSuite extends DisciplineSuite with CatsCompat:
  import StateGenerators.given
  import StateEq.given

  checkAll(
    "State[Int, Int]",
    FunctorTests[[A] =>> State[Int, A]].functor[Int, Int, Int]
  )

  checkAll(
    "State[Int, Int]",
    ApplicativeTests[[A] =>> State[Int, A]].applicative[Int, Int, Int]
  )

  checkAll(
    "State[Int, Int]",
    MonadTests[[A] =>> State[Int, A]].stackUnsafeMonad[Int, Int, Int]
  )
