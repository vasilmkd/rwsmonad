package rwsmonad

object ReaderExercises:
  def const[A, B](b: B): A => B = _ => b

  final case class ABConfig(doNotUseLetterE: Boolean, doNotUseLetterL: Boolean)

  def toUpperStr(str: String): Reader[ABConfig, String] =
    for
      cfg <- Reader.ask[ABConfig]

      filters: List[Char => Boolean] = List(
        if cfg.doNotUseLetterE then (_ != 'E') else const(true),
        if cfg.doNotUseLetterL then (_ != 'L') else const(true)
      )

      passesFilters: (Char => Boolean) = c => filters.forall(f => f(c))
    yield str.map(_.toUpper).filter(passesFilters)

  def welcomeMessage(motd: String, username: String): Reader[ABConfig, String] =
    for
      upperUser <- toUpperStr(username)
      upperMotd <- toUpperStr(motd)
    yield s"Welcome, $upperUser! Message of the day: $upperMotd"

  def fullName(name: String, surname: String, nickname: String): Reader[ABConfig, String] =
    for
      upperName <- toUpperStr(name)
      upperNick <- toUpperStr(nickname)
      upperSurname <- toUpperStr(surname)
    yield s"""$upperName "$upperNick" $upperSurname"""
