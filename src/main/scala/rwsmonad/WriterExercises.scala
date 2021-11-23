package rwsmonad

object WriterExercises:
  def addTwo(n: Int): Writer[List[String], Int] =
    for _ <- Writer.tell(List("adding 2..."))
    yield n + 2

  def augmentAndStringify(x: Int, y: Int): Writer[List[String], String] =
    for
      _ <- Writer.tell[List[String]](List("augmenting..."))
      newX <- addTwo(x)
      newY <- addTwo(y)
      _ <- Writer.tell[List[String]](List("stringifying..."))
    yield s"$newX $newY"
