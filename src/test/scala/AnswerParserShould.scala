import org.scalatest._
import PollDeps._
import PollDeps.Parsers.AnswerParser

class AnswerParserShould extends FlatSpec with Matchers {
  "AnswerParser" should "parse answer in all positive cases" in {
    AnswerParser
      .parse(AnswerParser.definition, "(1) (Answer is 42)")
      .map(answer => {
        info(answer.toString)
        answer.questionOneBasedIndex shouldBe 1
        answer.stringAnswer.nonEmpty shouldBe true
        answer.stringAnswer.get shouldBe "Answer is 42"
        answer.indexAnswers shouldBe None
      }).getOrElse(Failed) shouldBe Succeeded

    AnswerParser
      .parse(AnswerParser.definition, "(2) (1) (2) (3)")
      .map(answer => {
        info(answer.toString)
        answer.questionOneBasedIndex shouldBe 2
        answer.stringAnswer shouldBe None
        answer.indexAnswers.nonEmpty shouldBe true
        answer.indexAnswers.get shouldEqual List(1, 2, 3)
      }).getOrElse(Failed) shouldBe Succeeded
  }
}