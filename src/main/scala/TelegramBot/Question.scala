package TelegramBot

import scala.util.Try

case class Question(question: String, kind: QuestionKind, answers: Map[String, (Int, List[Int])],
                    options: Option[List[String]] = None) {
  def withAnswer(answer: Answer, user: Option[Int] = None): Either[String, Question] =
    kind match {
      case Open =>
        if (answer.options.length != 1) Left("Wrong count of options for OPEN question (expected 1)")
        else {
          val option = answer.options.head
          val users = answers.getOrElse(option, (0, List.empty))
          Right(this.copy(answers = answers + (option -> getUpdatedUsers(users, user))))
        }
      case Choice =>
        if (answer.options.length != 1) Left("Wrong count of options for CHOICE question (expected 1)")
        else mapNumericOption(answer.options.head)
          .map(option => answers.get(option)
            .map(users => Right(this.copy(answers = answers + (option -> getUpdatedUsers(users, user)))))
            .getOrElse(Left("FATAL:KEY GET LOST")))
          .getOrElse(Left("Question with passed number does not exist"))
      case Multi => {
        val mappedOptions = answer.options.flatMap(mapNumericOption)
        if (mappedOptions.size != answer.options.size) Left("Some options are incorrect")
        else if (mappedOptions.toSet.size != mappedOptions.size) Left("Repeating answers forbidden")
        else Right(this.copy(answers = getUpdatedAnswers(mappedOptions, user, answers)))
      }}

  def getUpdatedAnswers(options: List[String], user: Option[Int],
                        answers: Map[String, (Int, List[Int])]): Map[String, (Int, List[Int])] =
    if (options.isEmpty) answers
    else getUpdatedAnswers(options.drop(1), user,
      answers + (options.head -> getUpdatedUsers(answers.getOrElse(options.head, (0, List.empty)), user)))

  def getUpdatedUsers(users: (Int, List[Int]), user: Option[Int]): (Int, List[Int]) =
    (users._1 + 1, user.map(id => id :: users._2).getOrElse(List.empty))

  def mapNumericOption(numeric: String): Option[String] =
    options.flatMap(list => Try(list(numeric.toInt - 1)).toOption)

  def headerText: String = s"$question [$kind]"

  def asText: String =
    headerText + options.getOrElse(List.empty).zipWithIndex.map {
      case (option, i) => s"\n\t(${i + 1}) $option"
    }.mkString("")
}