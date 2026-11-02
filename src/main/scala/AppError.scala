sealed trait AppError:
  def message: String

case class ConfigError(message: String) extends AppError
case class ValidationError(message: String) extends AppError
case class GenerationError(message: String) extends AppError
case class CommandError(message: String) extends AppError
case class LoggingError(message: String) extends AppError