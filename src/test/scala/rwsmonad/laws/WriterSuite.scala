package rwsmonad
package laws

import cats.laws.discipline.*
import munit.DisciplineSuite

class WriterSuite extends DisciplineSuite with CatsCompat:
  import WriterGenerators.given
  import WriterEq.given

  checkAll("Writer[Int, Int]", FunctorTests[[A] =>> Writer[Int, A]].functor[Int, Int, Int])

  checkAll("Writer[Int, Int]", ApplicativeTests[[A] =>> Writer[Int, A]].applicative[Int, Int, Int])

  checkAll("Writer[Int, Int]", MonadTests[[A] =>> Writer[Int, A]].stackUnsafeMonad[Int, Int, Int])
