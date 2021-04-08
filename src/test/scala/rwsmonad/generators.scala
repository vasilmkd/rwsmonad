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

      private def genGet[A: Arbitrary: Cogen]: Gen[State[S, A]] =
        for
          a <- arbitrary[A]
        yield State.get.map(_ => a)

      private def genPut[A: Arbitrary: Cogen]: Gen[State[S, A]] =
        for
          s <- arbitrary[S]
          a <- arbitrary[A]
        yield State.put(s).map(_ => a)

      private def genModify[A: Arbitrary: Cogen]: Gen[State[S, A]] =
        for
          f <- arbitrary[S => S]
          a <- arbitrary[A]
        yield State.modify(f).map(_ => a)

  given [S: Arbitrary: Cogen, A: Arbitrary: Cogen]: Arbitrary[State[S, A]] =
    Arbitrary(generators[S].generators[A])
