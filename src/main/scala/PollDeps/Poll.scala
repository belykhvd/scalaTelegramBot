package PollDeps

import org.joda.time.DateTime

import scala.collection.mutable

case class Poll
  (name: String,
   anonymous: Boolean = false,
   visibility: ResultsVisibility = Continuous,
   startTime: Option[DateTime] = None,
   endTime: Option[DateTime] = None,
   status: PollStatus = NotStarted,
   questions: List[Question] = List.empty[Question]) {

  // questionNumber -> List[(userId, Answer)]
  var answers = Map.empty[Int, List[(Int, Answer)]]

  def getStatus: PollStatus =
    if (status == Stopped) Stopped
    else {
      val isRunning = (status == Running || startTime.exists(time => time.isBeforeNow || time.isEqualNow)) &&
        endTime.forall(time => time.isAfterNow)
      if (isRunning) Running
      else NotStarted
    }

  def started: Poll =
    this.copy(status = Running)

  def stopped: Poll =
    this.copy(status = Stopped)

  def withQuestion(question: Question): Poll =
    this.copy(questions = questions :+ question)

  def withoutQuestion(oneBasedIndex: Int): Poll =
    this.copy(questions = questions.take(oneBasedIndex - 1) ++ questions.drop(oneBasedIndex))

  def asText: String = {
    val builder = mutable.StringBuilder.newBuilder
    for (i <- questions.indices)
      builder.append(s"${i + 1}. ${questions(i).asText}\n")
    if (builder.nonEmpty) builder.toString
    else "Poll is empty for now."
  }

  override def toString: String =
    s"name: [$name] anonymous: [$anonymous] visibility: [$visibility] " +
      s"startTime: [$startTime] endTime: [$endTime] status: [$getStatus]"
}