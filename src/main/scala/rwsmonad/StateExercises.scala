package rwsmonad

object StateExercises:
  def reverseWithCount[A](as: List[A]): State[Int, List[A]] =
    for _ <- State.modify[Int](_ + 1)
    yield as.reverse

  def appendReversedWithCount[A](
      as1: List[A],
      as2: List[A]
  ): State[Int, List[A]] =
    for
      rev1 <- reverseWithCount(as1)
      rev2 <- reverseWithCount(as2)
    yield rev1 ++ rev2

  def append3ReversedWithCount[A](
      as1: List[A],
      as2: List[A],
      as3: List[A]
  ): State[Int, List[A]] =
    for
      rev1 <- reverseWithCount(as1)
      rev2 <- reverseWithCount(as2)
      rev3 <- reverseWithCount(as3)
    yield rev1 ++ rev2 ++ rev3
