package TelegramBot

object ResponseConstants {
  def pollCreated(id: Long) = s"Succeeded: Poll $id created."
  def pollStarted(id: Long) = s"Succeeded: Poll $id started."

  def begin(id: Long) = s"Succeeded: You start working with poll $id."
  def end(id: Long) = s"Succeeded: You stop working with poll $id."
  def addedQuestion = s"Succeeded: Question added."
  def deletedQuestion = s"Succeeded: Question deleted."
  def answerRecorded = s"Succeeded: Answer recorded."

  def pollStopped(id: Long) = s"Succeeded: Poll $id stopped."
  def pollDeleted(id: Long) = s"Succeeded: Poll $id deleted."
}