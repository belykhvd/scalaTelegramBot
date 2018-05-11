import org.scalatest._
import TelegramBot.Parsers.AnswerParser

class AnswerParserShould extends FlatSpec with Matchers {
  "AnswerParser" should "parse answer in all positive cases" in {
    AnswerParser.parse(AnswerParser.definition, "(1) (Answer is 42)")
      .map(answer => {
        info(answer.toString)
        answer.number shouldBe 1
        answer.options shouldEqual List("Answer is 42")
      }).getOrElse(Failed) shouldBe Succeeded

    AnswerParser.parse(AnswerParser.definition, "(2) (1) (2) (3)")
      .map(answer => {
        info(answer.toString)
        answer.number shouldBe 2
        answer.options shouldEqual List("1", "2", "3")
      }).getOrElse(Failed) shouldBe Succeeded
  }
}