object Validator:

  def validateConfig(config: Config): Either[AppError, Unit] =
    if config.defaultPolicy.isEmpty then
      Left(ValidationError("Не указана политика по умолчанию"))
    else if !config.policies.contains(config.defaultPolicy) then
      Left(ValidationError("Политика по умолчанию отсутствует в списке политик"))
    else if config.policies.isEmpty then
      Left(ValidationError("Список политик пуст"))
    else
      config.policies.toList
        .map((name, policy) => validatePolicy(name, policy))
        .collectFirst {
          case Left(error) => error
        }
        .map(Left(_))
        .getOrElse(Right(()))

  private def validatePolicy(name: String, policy: Policy): Either[AppError, Unit] =
    if policy.minimumLength < 1 then
      Left(ValidationError(s"Политика '$name': минимальная длина должна быть больше 0"))
    else if policy.maximumLength < policy.minimumLength then
      Left(ValidationError(s"Политика '$name': максимальная длина меньше минимальной"))
    else if policy.defaultLength < policy.minimumLength ||
      policy.defaultLength > policy.maximumLength then
      Left(ValidationError(s"Политика '$name': длина по умолчанию вне допустимого диапазона"))
    else if policy.characterGroups.isEmpty then
      Left(ValidationError(s"Политика '$name': не включена ни одна группа символов"))
    else if policy.characterGroups.values.exists(_.minimumCount < 0) then
      Left(ValidationError(s"Политика '$name': минимальное количество не может быть отрицательным"))
    else if hasDisabledGroupWithMinimumCount(policy) then
      Left(ValidationError(s"Политика '$name': у отключённой группы задано обязательное количество"))
    else if mandatoryCount(policy) > policy.maximumLength then
      Left(ValidationError(s"Политика '$name': сумма обязательных символов больше максимальной длины"))
    else if hasForbiddenAllowedCharacter(policy) then
      Left(ValidationError(s"Политика '$name': разрешённый символ находится в списке запрещённых"))
    else if hasEmptyEnabledGroup(policy) then
      Left(ValidationError(s"Политика '$name': включённая группа не содержит доступных символов"))
    else if !policy.allowRepeatedCharacters &&
      availableCharacters(policy).length < policy.maximumLength then
      Left(ValidationError(s"Политика '$name': недостаточно символов для пароля без повторений"))
    else if policy.maximumConsecutiveIdenticalCharacters < 1 then
      Left(ValidationError(s"Политика '$name': максимальное число повторений должно быть больше 0"))
    else
      Right(())

  private def hasDisabledGroupWithMinimumCount(policy: Policy): Boolean =
    policy.characterGroups.values.exists(group =>
      !group.enabled && group.minimumCount > 0
    )

  private def mandatoryCount(policy: Policy): Int =
    policy.characterGroups.values
      .filter(_.enabled)
      .map(_.minimumCount)
      .sum

  private def availableCharacters(policy: Policy): String =
    policy.characterGroups.toList
      .filter((_, group) => group.enabled)
      .map((name, group) =>
        group.characters.getOrElse(
          name match
            case "lowercase" => "abcdefghijklmnopqrstuvwxyz"
            case "uppercase" => "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
            case "digits" => "0123456789"
            case _ => ""
        )
      )
      .mkString
      .filterNot(policy.forbiddenCharacters.contains)

  private def hasForbiddenAllowedCharacter(policy: Policy): Boolean =
    availableCharacters(policy).exists(
      policy.forbiddenCharacters.contains
    )

  private def hasEmptyEnabledGroup(policy: Policy): Boolean =
    policy.characterGroups.toList
      .filter((_, group) => group.enabled)
      .exists { case (name, group) =>
        val characters =
          group.characters.getOrElse(
            name match
              case "lowercase" => "abcdefghijklmnopqrstuvwxyz"
              case "uppercase" => "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
              case "digits" => "0123456789"
              case _ => ""
          )

        characters.filterNot(policy.forbiddenCharacters.contains).isEmpty
      }