/*
 * The following code has been shamelessly taken from Cats Effect 3. The
 * original code can be found here:
 * https://github.com/typelevel/cats-effect/blob/1cc8eec66a170e6befca7cf457d6989bc546dba6/kernel-testkit/shared/src/main/scala/cats/effect/kernel/testkit/Generators.scala
 */

package rwsmonad

import org.scalacheck.{Arbitrary, Cogen, Gen}, Arbitrary.arbitrary

import scala.collection.immutable.SortedMap

trait GenK[F[_]]:
  def apply[A: Arbitrary: Cogen]: Gen[F[A]]

trait Generators1[F[_]]:
  protected val maxDepth: Int = 10
  
  protected def baseGen[A: Arbitrary: Cogen]: List[(String, Gen[F[A]])] = Nil

  protected def recursiveGen[A: Arbitrary: Cogen](deeper: GenK[F]): List[(String, Gen[F[A]])] = Nil

  private def gen[A: Arbitrary: Cogen](depth: Int): Gen[F[A]] =
    val genK: GenK[F] =
      new GenK[F]:
        def apply[A: Arbitrary: Cogen]: Gen[F[A]] = Gen.delay(gen(depth + 1))

    val gens =
      if depth > maxDepth then baseGen
      else baseGen ++ recursiveGen(genK)

    Gen.oneOf(SortedMap(gens: _*).map(_._2)).flatMap(identity)

  def generators[A: Arbitrary: Cogen]: Gen[F[A]] = gen[A](0)

trait FunctorGenerators[F[_]] extends Generators1[F]:
  given F: Functor[F]

  override protected def recursiveGen[A: Arbitrary: Cogen](deeper: GenK[F]): List[(String, Gen[F[A]])] =
    ("map", genMap(deeper)) :: super.recursiveGen(deeper)

  private def genMap[A: Arbitrary: Cogen](deeper: GenK[F]): Gen[F[A]] =
    for
      fa <- deeper[A]
      f <- arbitrary[A => A]
    yield fa.map(f)

trait ApplicativeGenerators[F[_]] extends FunctorGenerators[F]:
  override given F: Applicative[F]

  override protected def baseGen[A: Arbitrary: Cogen]: List[(String, Gen[F[A]])] =
    ("pure", genPure) :: super.baseGen

  override protected def recursiveGen[A: Arbitrary: Cogen](deeper: GenK[F]): List[(String, Gen[F[A]])] =
    ("ap", genAp(deeper)) :: super.recursiveGen(deeper)

  private def genPure[A: Arbitrary]: Gen[F[A]] =
    arbitrary[A].map(F.pure)

  private def genAp[A: Arbitrary: Cogen](deeper: GenK[F]): Gen[F[A]] =
    for
      fa <- deeper[A]
      ff <- deeper[A => A]
    yield ff.ap(fa)

trait MonadGenerators[F[_]] extends ApplicativeGenerators[F]:
  override given F: Monad[F]

  override protected def recursiveGen[A: Arbitrary: Cogen](deeper: GenK[F]): List[(String, Gen[F[A]])] =
    ("flatMap", genFlatMap(deeper)) :: super.recursiveGen(deeper)

  private def genFlatMap[A: Arbitrary: Cogen](deeper: GenK[F]): Gen[F[A]] =
    for
      fa <- deeper[A]
      f <- Gen.function1[A, F[A]](deeper[A])
    yield fa.flatMap(f)

object StateGenerators:
  def generators[S: Arbitrary: Cogen]: MonadGenerators[[A] =>> State[S, A]] =
    new MonadGenerators[[A] =>> State[S, A]]:
      given F: Monad[[A] =>> State[S, A]] = State.given_Monad_State

      override protected def baseGen[A: Arbitrary: Cogen]: List[(String, Gen[State[S, A]])] =
        List(
          ("get", genGet),
          ("put", genPut),
          ("modify", genModify)
        ) ++ super.baseGen

      private def genGet[A: Arbitrary]: Gen[State[S, A]] =
        for
          a <- arbitrary[A]
        yield State.get.map(_ => a)

      private def genPut[A: Arbitrary]: Gen[State[S, A]] =
        for
          s <- arbitrary[S]
          a <- arbitrary[A]
        yield State.put(s).map(_ => a)

      private def genModify[A: Arbitrary]: Gen[State[S, A]] =
        for
          f <- arbitrary[S => S]
          a <- arbitrary[A]
        yield State.modify(f).map(_ => a)

  given [S: Arbitrary: Cogen, A: Arbitrary: Cogen]: Arbitrary[State[S, A]] =
    Arbitrary(generators[S].generators[A])

object ReaderGenerators:
  def generators[R: Arbitrary: Cogen]: MonadGenerators[[A] =>> Reader[R, A]] =
    new MonadGenerators[[A] =>> Reader[R, A]]:
      given F: Monad[[A] =>> Reader[R, A]] = Reader.given_Monad_Reader

      override protected def baseGen[A: Arbitrary: Cogen]: List[(String, Gen[Reader[R, A]])] =
        List(
          ("ask", genAsk),
          ("asks", genAsks)
        ) ++ super.baseGen

      override protected def recursiveGen[A: Arbitrary: Cogen](deeper: GenK[[A] =>> Reader[R, A]]): List[(String, Gen[Reader[R, A]])] =
        ("local", genLocal[A](deeper)) :: super.recursiveGen(deeper)

      private def genAsk[A: Arbitrary]: Gen[Reader[R, A]] =
        for
          a <- arbitrary[A]
        yield Reader.ask.map(_ => a)

      private def genAsks[A: Arbitrary]: Gen[Reader[R, A]] =
        for
          f <- arbitrary[R => A]
        yield Reader.asks(f)

      private def genLocal[A: Arbitrary: Cogen](deeper: GenK[[A] =>> Reader[R, A]]): Gen[Reader[R, A]] =
        for
          fa <- deeper[A]
          f <- arbitrary[R => R]
        yield Reader.local(fa)(f)

  given [R: Arbitrary: Cogen, A: Arbitrary: Cogen]: Arbitrary[Reader[R, A]] =
    Arbitrary(generators[R].generators[A])

object WriterGenerators:
  def generators[L: Monoid: Arbitrary: Cogen]: MonadGenerators[[A] =>> Writer[L, A]] =
    new MonadGenerators[[A] =>> Writer[L, A]]:
      given F: Monad[[A] =>> Writer[L, A]] = Writer.given_Monad_Writer

      override protected def baseGen[A: Arbitrary: Cogen]: List[(String, Gen[Writer[L, A]])] =
        ("tell", genTell) :: super.baseGen

      override protected def recursiveGen[A: Arbitrary: Cogen](
          deeper: GenK[[A] =>> Writer[L, A]]): List[(String, Gen[Writer[L, A]])] =
        List(
          ("censor", genCensor(deeper)),
          ("listen", genListen(deeper))
        ) ++ super.recursiveGen(deeper)

      private def genTell[A: Arbitrary]: Gen[Writer[L, A]] =
        for
          l <- arbitrary[L]
          a <- arbitrary[A]
        yield Writer.tell(l).map(_ => a)

      private def genCensor[A: Arbitrary: Cogen](deeper: GenK[[A] =>> Writer[L, A]]): Gen[Writer[L, A]] =
        for
          fa <- deeper[A]
          f <- arbitrary[L => L]
        yield Writer.censor(fa)(f)

      private def genListen[A: Arbitrary: Cogen](deeper: GenK[[A] =>> Writer[L, A]]): Gen[Writer[L, A]] =
        for
          fa <- deeper[A]
        yield Writer.listen(fa).map(_._1)

  given [L: Monoid: Arbitrary: Cogen, A: Arbitrary: Cogen]: Arbitrary[Writer[L, A]] =
    Arbitrary(generators[L].generators[A])
