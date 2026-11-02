import java.io.FileWriter
import java.time.LocalDateTime

object Logger:

  def log(file: String, message: String): Either[AppError, Unit] =
    try
      val writer = new FileWriter(file, true)

      try {
        writer.write(s"${LocalDateTime.now()} - $message")
        writer.write(System.lineSeparator())
        Right(())
      } finally
        writer.close()

    catch
      case e: Exception =>
        Left(LoggingError(s"Ошибка записи в лог: ${e.getMessage}"))