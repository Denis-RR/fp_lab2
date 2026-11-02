import scala.util.Random

object PasswordService:
  private val lowercase = "abcdefghijklmnopqrstuvwxyz"
  private val uppercase = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  private val digits = "0123456789"

  def generate(policy: Policy, length: Int): Either[AppError, String] =
    if length < policy.minimumLength || length > policy.maximumLength then
      Left(GenerationError(s"Длина пароля должна быть от ${policy.minimumLength} до ${policy.maximumLength}"))
    else
      val mandatoryCharacters = getMandatoryCharacters(policy)

      if mandatoryCharacters.length > length then
        Left(GenerationError("Невозможно создать пароль заданной длины"))
      else
        val available = availableCharacters(policy)

        if available.isEmpty then
          Left(GenerationError("Политика не содержит доступных символов"))
        else
          val remainingLength = length - mandatoryCharacters.length
          val additionalCharacters =
            Random.shuffle(available.toList)
              .filter(character =>
                policy.allowRepeatedCharacters ||
                  !mandatoryCharacters.contains(character)
              )
              .take(remainingLength)

          val result =
            Random.shuffle(
              mandatoryCharacters ++ additionalCharacters
            ).mkString

          if result.length != length then
            Left(GenerationError("Невозможно создать пароль с заданными ограничениями"))
          else if isValid(result, policy) then
            Right(result)
          else
            generateWithAttempts(policy, length, 100)

  private def generateWithAttempts(policy: Policy, length: Int, attempts: Int): Either[AppError, String] =
    if attempts <= 0 then
      Left(GenerationError("Не удалось сгенерировать пароль с заданными ограничениями"))
    else
      val mandatoryCharacters = getMandatoryCharacters(policy)
      val available = availableCharacters(policy)

      val result =
        Random.shuffle(
          mandatoryCharacters ++
            Random.shuffle(available.toList)
              .filter(character =>
                policy.allowRepeatedCharacters ||
                  !mandatoryCharacters.contains(character)
              )
              .take(length - mandatoryCharacters.length)
        ).mkString

      if result.length == length && isValid(result, policy) then
        Right(result)
      else
        generateWithAttempts(policy, length, attempts - 1)

  private def getMandatoryCharacters(policy: Policy): List[Char] =
    policy.characterGroups.toList
      .filter((_, group) => group.enabled)
      .flatMap { case (name, group) =>
        val characters = getGroupCharacters(name, group).filterNot(policy.forbiddenCharacters.contains)

        if policy.allowRepeatedCharacters then
          List.fill(group.minimumCount) {
            characters(Random.nextInt(characters.length))
          }
        else
          Random.shuffle(characters.toList).take(group.minimumCount)
      }

  private def availableCharacters(policy: Policy): String =
    policy.characterGroups.toList
      .filter((_, group) => group.enabled)
      .map { case (name, group) =>
        getGroupCharacters(name, group)
      }
      .mkString
      .filterNot(policy.forbiddenCharacters.contains)

  private def getGroupCharacters(name: String, group: CharacterGroup): String =
    group.characters.getOrElse(
      name match
        case "lowercase" => lowercase
        case "uppercase" => uppercase
        case "digits" => digits
        case _ => ""
    )

  def isValid(password: String, policy: Policy): Boolean =
    password.length >= policy.minimumLength &&
      password.length <= policy.maximumLength &&
      checkGroups(password, policy) &&
      checkForbiddenCharacters(password, policy) &&
      checkRepeatedCharacters(password, policy) &&
      checkConsecutiveCharacters(password, policy) &&
      checkSpaces(password, policy) &&
      checkForbiddenSubstrings(password, policy)

  private def checkGroups(password: String, policy: Policy): Boolean =
    policy.characterGroups.forall { case (name, group) =>
      if !group.enabled then
        true
      else
        val characters = getGroupCharacters(name, group)
        password.count(characters.contains) >= group.minimumCount
    }

  private def checkForbiddenCharacters(password: String, policy: Policy): Boolean =
    password.forall(ch => !policy.forbiddenCharacters.contains(ch))

  private def checkRepeatedCharacters(password: String, policy: Policy): Boolean =
    policy.allowRepeatedCharacters ||
      password.distinct.length == password.length

  private def checkConsecutiveCharacters(password: String, policy: Policy): Boolean =
    if password.isEmpty then
      true
    else
      password.tail
        .foldLeft((password.head, 1, true)) {
          case ((previous, count, valid), current) =>
            if !valid then
              (current, count, false)
            else if current == previous then
              val newCount = count + 1

              (
                current,
                newCount,
                newCount <= policy.maximumConsecutiveIdenticalCharacters
              )
            else
              (current, 1, true)
        }
        ._3

  private def checkSpaces(password: String, policy: Policy): Boolean =
    policy.allowSpaces || !password.exists(_.isWhitespace)

  private def checkForbiddenSubstrings(password: String, policy: Policy): Boolean =
    policy.forbiddenSubstrings.forall(
      substring => !password.contains(substring)
    )

  def checkPassword(password: String, policy: Policy): Map[String, Boolean] =
    Map(
      "Длина" -> (password.length >= policy.minimumLength && password.length <= policy.maximumLength),
      "Группы символов" -> checkGroups(password, policy),
      "Запрещённые символы" -> checkForbiddenCharacters(password, policy),
      "Повторяющиеся символы" -> checkRepeatedCharacters(password, policy),
      "Одинаковые символы подряд" -> checkConsecutiveCharacters(password, policy),
      "Пробелы" -> checkSpaces(password, policy),
      "Запрещённые подстроки" -> checkForbiddenSubstrings(password, policy)
    )