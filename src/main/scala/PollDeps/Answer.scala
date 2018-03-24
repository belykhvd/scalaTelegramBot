package PollDeps

case class Answer(questionOneBasedIndex: Int, stringAnswer: Option[String] = None, indexAnswers: Option[List[Int]] = None) {
  override def toString: String = s"$questionOneBasedIndex $stringAnswer $indexAnswers"
}