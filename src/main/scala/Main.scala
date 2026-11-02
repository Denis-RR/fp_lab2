import scala.annotation.tailrec
import scala.io.StdIn
import scala.util.{Failure, Success, Try}

object Main:

  private val configPath = "config.json"

  private sealed trait MenuNode:
    def show: String

  private case class MenuLeaf(name: String, command: () => Option[Command]) extends MenuNode:
    override def show: String = name

  private case class MenuContainer(name: String, content: Seq[MenuNode]) extends MenuNode:
    override def show: String = name

  private case class AppState(config: Config, running: Boolean)
  private def executeCommand(command: Command, state: AppState): AppState =
    command match
      case ShowHelp =>
        showHelp()
        state

      case ShowPolicies =>
        showPolicies(state.config)
        state

      case ShowPolicy(name) =>
        showPolicy(state.config, name)
        state

      case GeneratePassword(name, length) =>
        generate(state.config, name, length)
        state

      case CheckPassword(name, password) =>
        checkPassword(state.config, name, password)
        state

      case ReloadConfig =>
        state.copy(config = reloadConfig(state.config))

      case Exit =>
        state.copy(running = false)

  def main(args: Array[String]): Unit =
    ConfigLoader.load(configPath) match
      case Left(error) =>
        println(s"Ошибка загрузки конфигурации: ${error.message}")

      case Right(config) =>
        Validator.validateConfig(config) match
          case Left(error) =>
            println(s"Ошибка конфигурации: ${error.message}")

          case Right(_) =>
            run(config)

  private def run(initialConfig: Config): Unit =
    val initialState = AppState(initialConfig, running = true)

    val menu = MenuContainer(
      "ГЕНЕРАТОР ПАРОЛЕЙ",
      Seq(
        MenuContainer("Генерация пароля",
          Seq(
            MenuLeaf("Сгенерировать пароль стандартной длины",
              () => askPolicyName().map(name => GeneratePassword(name, None))
            ),
            MenuLeaf("Сгенерировать пароль заданной длины",
              () => askPolicyName().flatMap { name =>
                readLength().map { length =>
                  GeneratePassword(name, Some(length))
                }
              }
            )
          )
        ),
        MenuContainer("Проверка пароля",
          Seq(
            MenuLeaf("Проверить пароль",
              () => askPolicyName().flatMap { name =>
                println("Введите пароль:")
                readInput().map { password =>
                  CheckPassword(name, password)
                }
              }
            )
          )
        ),
        MenuContainer("Просмотр политик",
          Seq(
            MenuLeaf("Показать все политики",
              () => Some(ShowPolicies)
            ),
            MenuLeaf("Посмотреть подробности политики",
              () =>
                askPolicyName().map(name => ShowPolicy(name))
            )
          )
        ),
        MenuLeaf("Перезагрузить конфигурацию",
          () => Some(ReloadConfig)
        ),
        MenuLeaf("Справка",
          () => Some(ShowHelp)
        ),
        MenuLeaf("Выход",
          () => Some(Exit)
        )
      )
    )
    menuLoop(menu, Seq.empty, initialState)
    println("Программа завершена")

  // функция для навигации по меню
  @tailrec
  private def menuLoop(menuContainer: MenuContainer, prevs: Seq[MenuContainer], state: AppState): AppState =
    if !state.running then
      state
    else
      println()
      println(s"    ${menuContainer.name}")
      val content = menuContainer.content

      content.zipWithIndex.foreach { case (node, index) =>
        println(s"${index + 1}. ${node.show}")
      }
      if prevs.nonEmpty then
        println(s"${content.length + 1}. Назад")
      println("Выберите пункт:")
      val maxChoice = content.length + (if prevs.nonEmpty then 1 else 0)

      readMenuChoice(maxChoice) match
        case None => state
        case Some(choice)
          if prevs.nonEmpty && choice == content.length + 1 =>
          menuLoop(prevs.last, prevs.dropRight(1), state)

        case Some(choice) =>
          content(choice - 1) match
            case mc: MenuContainer =>
              menuLoop(mc, prevs.appended(menuContainer), state)

            case MenuLeaf(_, commandBuilder) =>
              commandBuilder() match
                case None =>
                  menuLoop(menuContainer, prevs, state)

                case Some(command) =>
                  val newState = executeCommand(command, state)
                  menuLoop(menuContainer, prevs, newState)

  @tailrec
  private def readMenuChoice(maxChoice: Int): Option[Int] =
    val input = StdIn.readLine()

    if input == null then
      None
    else
      Try(input.trim.toInt) match
        case Failure(_) =>
          println("Введите целое число:")
          readMenuChoice(maxChoice)

        case Success(value) =>
          if value < 1 || value > maxChoice then
            println("Такого пункта нет. Попробуйте ещё раз:")
            readMenuChoice(maxChoice)
          else
            Some(value)

  private def readInput(): Option[String] =
    Option(StdIn.readLine())

  private def askPolicyName(): Option[String] =
    println("Введите название политики:")
    readInput().map(_.trim).filter(_.nonEmpty)

  @tailrec
  private def readLength(): Option[Int] =
    println("Введите длину пароля:")
    Option(StdIn.readLine()) match
      case None => None
      case Some(input) =>
        input.trim.toIntOption match
          case None =>
            println("Ошибка: длина должна быть целым числом.")
            readLength()
          case Some(value) => Some(value)

  private def reloadConfig(currentConfig: Config): Config =
    ConfigLoader.load(configPath) match
      case Left(error) =>
        println(s"Ошибка перезагрузки: ${error.message}")
        Logger.log(currentConfig.logFile, "Ошибка перезагрузки конфигурации")
        currentConfig

      case Right(newConfig) =>
        Validator.validateConfig(newConfig) match
          case Left(error) =>
            println(s"Ошибка конфигурации: ${error.message}")
            Logger.log(currentConfig.logFile, "Перезагрузка отклонена: некорректная конфигурация")
            currentConfig

          case Right(_) =>
            println("Конфигурация успешно перезагружена.")
            Logger.log(newConfig.logFile, "Конфигурация успешно перезагружена")
            newConfig

  private def showHelp(): Unit =
    println(
      """Для работы с программой:
        |1. Выберите нужный раздел меню.
        |2. Выберите действие внутри раздела.
        |3. Для возврата в предыдущий раздел выберите "Назад".
        |4. Для завершения программы выберите "Выход".
        |""".stripMargin
    )

  private def showPolicies(config: Config): Unit =
    println("Доступные политики:")
    config.policies.foreach { case (name, policy) =>
      println(
        s"- $name: ${policy.description} " +
          s"(длина ${policy.minimumLength}-${policy.maximumLength})"
      )
    }

  private def showPolicy(config: Config, name: String): Unit =
    config.policies.get(name) match
      case None =>
        println(s"Политика '$name' не найдена.")

      case Some(policy) =>
        println(s"Политика: $name")
        println(s"Описание: ${policy.description}")
        println(s"Длина: ${policy.minimumLength}-${policy.maximumLength}")
        println(s"Длина по умолчанию: ${policy.defaultLength}")
        println(s"Повторяющиеся символы: ${policy.allowRepeatedCharacters}")
        println(s"Максимум одинаковых подряд: " + s"${policy.maximumConsecutiveIdenticalCharacters}")
        println(s"Пробелы разрешены: ${policy.allowSpaces}")
        println(s"Запрещённые символы: ${policy.forbiddenCharacters}")
        println(s"Запрещённые подстроки: " + s"${policy.forbiddenSubstrings.mkString(", ")}")
        println("Группы символов:")
        policy.characterGroups.foreach { case (groupName, group) =>
          println(s"- $groupName: включена=${group.enabled}, " + s"минимум=${group.minimumCount}")
        }

  private def generate(config: Config, name: String, requestedLength: Option[Int]): Unit =
    config.policies.get(name) match
      case None =>
        println(s"Политика '$name' не найдена.")

      case Some(policy) =>
        val length = requestedLength.getOrElse(policy.defaultLength)

        PasswordService.generate(policy, length) match
          case Left(error) =>
            println(s"Ошибка генерации: ${error.message}")
            Logger.log(config.logFile, s"Ошибка генерации по политике '$name'")

          case Right(password) =>
            println(s"Пароль: $password")
            Logger.log(config.logFile, s"Сгенерирован пароль по политике '$name'")

  private def checkPassword(config: Config, name: String, password: String): Unit =
    config.policies.get(name) match
      case None =>
        println(s"Политика '$name' не найдена.")

      case Some(policy) =>
        println(s"Проверка пароля по политике '$name':")
        val results = PasswordService.checkPassword(password, policy)
        results.foreach {
          case (req , result) => printResult(req, result)
        }
        Logger.log(config.logFile, s"Проверен пароль по политике '$name'")

  private def printResult(requirement: String, result: Boolean): Unit =
    if result then
      println(s"[OK] $requirement")
    else
      println(s"[FAIL] $requirement")