sealed trait Command
case object ShowHelp extends Command
case object ShowPolicies extends Command
case class ShowPolicy(name: String) extends Command
case class GeneratePassword(policyName: String, length: Option[Int]) extends Command
case class CheckPassword(policyName: String, password: String) extends Command
case object ReloadConfig extends Command
case object Exit extends Command