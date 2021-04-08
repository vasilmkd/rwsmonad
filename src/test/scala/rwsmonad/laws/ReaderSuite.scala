package rwsmonad
package laws

import cats.laws.discipline.*
import munit.DisciplineSuite

class ReaderSuite extends DisciplineSuite with CatsCompat:
  import ReaderGenerators.given
  import ReaderEq.given

  checkAll("Reader[Int, Int]", FunctorTests[[A] =>> Reader[Int, A]].functor[Int, Int, Int])

  checkAll("Reader[Int, Int]", ApplicativeTests[[A] =>> Reader[Int, A]].applicative[Int, Int, Int])

  checkAll("Reader[Int, Int]", MonadTests[[A] =>> Reader[Int, A]].stackUnsafeMonad[Int, Int, Int])
