import scala.io.Source
import upickle.default.read

object ConfigLoader:

  def load(path: String): Either[AppError, Config] =
    try
      val source = Source.fromFile(path)
      try
        Right(read[Config](source.mkString))
      finally
        source.close()

    catch
      case e: Exception =>
        Left(ConfigError(s"Не удалось загрузить конфигурацию: ${e.getMessage}"))