package TelegramBot

object ErrorConstants {
  val forbiddenViewResults: String = "Faulted: You may see poll results only when poll stopped."
  val unableUpdatePoll: String = "Faulted: You have no permissions or poll is running or stopped."
  val noPermissions: String = "Faulted: You have no permissions."
  val noPoll: String = "Faulted: Poll with passed ID does not exist."
  val noContext: String = "Faulted: Use \"begin <pollId>\" before applying operations."
  val badSyntax: String = "Faulted: Check command syntax."

  val fatalKeyGetLost: String = "FATAL:KEY GET LOST"
}