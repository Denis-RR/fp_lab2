import upickle.default.ReadWriter

case class CharacterGroup(
                           enabled: Boolean,
                           minimumCount: Int,
                           characters: Option[String] = None
                         ) derives ReadWriter

case class Policy(
                   description: String,
                   minimumLength: Int,
                   maximumLength: Int,
                   defaultLength: Int,
                   characterGroups: Map[String, CharacterGroup],
                   forbiddenCharacters: String,
                   allowRepeatedCharacters: Boolean,
                   maximumConsecutiveIdenticalCharacters: Int,
                   allowSpaces: Boolean,
                   forbiddenSubstrings: List[String]
                 ) derives ReadWriter

case class Config(
                   defaultPolicy: String,
                   logFile: String,
                   policies: Map[String, Policy]
                 ) derives ReadWriter