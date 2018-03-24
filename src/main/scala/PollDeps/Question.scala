package PollDeps

case class Question(question: String, kind: QuestionKind, options: Option[List[String]] = None) {
  def headerText: String = s"$question [$kind]"

  def asText: String = {
    val builder = StringBuilder.newBuilder.append(headerText)
    val optionsList = options.getOrElse(List.empty[String])
    for (i <- optionsList.indices)
      builder.append(s"\n\t(${i + 1}) ${optionsList(i)}")
    builder.toString
  }
}