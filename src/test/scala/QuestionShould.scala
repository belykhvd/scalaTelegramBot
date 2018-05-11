import TelegramBot._
import org.scalatest._

class QuestionShould extends FlatSpec with Matchers {
  "Question" should "be answered when OPEN" in {
    val openQuestion = Question("?", Open, Map.empty)
    val openAnswer = Answer(1, List("ANSWER"))
    openQuestion.withAnswer(openAnswer, Some(42))
      .fold(message => fail(message), question =>
        question.answers.getOrElse("ANSWER", (0, List.empty)) shouldBe (1, List(42)))
  }

  "Question" should "be answered when CHOICE" in {
    val choiceQuestion = Question("?", Choice, Map("A" -> (0, List.empty), "B" -> (0, List.empty)), Some(List("A", "B")))
    val choiceAnswer = Answer(1, List("1"))
    choiceQuestion.withAnswer(choiceAnswer)
      .fold(message => fail(message), question =>
        question.answers.getOrElse("A", (0, List.empty)) shouldBe (1, List.empty))
  }

  "Question" should "be answered when MULTI" in {
    val multiQuestion = Question("?", Multi, Map("A" -> (0, List.empty), "B" -> (0, List.empty)), Some(List("A", "B")))
    val multiAnswer = Answer(1, List("1", "2"))
    multiQuestion.withAnswer(multiAnswer)
      .fold(message => fail(message), question => {
        question.answers.getOrElse("A", (0, List.empty)) shouldBe (1, List.empty)
        question.answers.getOrElse("B", (0, List.empty)) shouldBe (1, List.empty)
      })
  }
}