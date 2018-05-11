package TelegramBot

import org.joda.time.DateTime

case class Poll
  (name: String,
   anonymous: Boolean = false,
   visibility: ResultsVisibility = Continuous,
   startTime: Option[DateTime] = None,
   endTime: Option[DateTime] = None,
   _status: PollStatus = NotStarted,
   creator: Option[Int] = None,
   id: Option[Long] = None,
   questions: List[Question] = List.empty) {

  def withAnswer(answer: Answer, user: Int): Either[String, Poll] =
    if (answer.number < 1 || answer.number > questions.length) Left("Incorrect question number")
    else {
      val index = answer.number - 1
      questions(index).withAnswer(answer, if (!anonymous) Some(user) else None)
        .fold(message => Left(message),
          question => Right(this.copy(questions = questions.take(index) ++ List(question) ++ questions.drop(index + 1))))
    }

  def status(now: DateTime): PollStatus =
    if (_status == Stopped) Stopped
    else {
      val isRunning = (_status == Running || startTime.exists(time => time.isBefore(now) || time.isEqual(now))) &&
        endTime.forall(time => time.isAfter(now))
      if (isRunning) Running
      else NotStarted
    }

  def started: Poll =
    this.copy(_status = Running)

  def stopped: Poll =
    this.copy(_status = Stopped)

  def withQuestion(question: Question): Poll =
    this.copy(questions = questions :+ question)

  def withoutQuestion(number: Int): Either[String, Poll] =
    if (number < 1 || number > questions.length) Left("Faulted: Passed index does not correspond to any question.")
    else Right(this.copy(questions = questions.take(number - 1) ++ questions.drop(number)))

  def asText: String = {
    val text = questions.indices.map(i => s"${i + 1}. ${questions(i).asText}").mkString("\n")
    if (text.nonEmpty) text
    else "Poll is empty for now."
  }

  override def toString: String =
    s"Poll ${id.getOrElse(-1)}:" +
      s"\n\tName: [$name] Anonymous: [$anonymous] Visibility: [$visibility] " +
      s"StartTime: [$startTime] EndTime: [$endTime] Status: [${_status}]"
}