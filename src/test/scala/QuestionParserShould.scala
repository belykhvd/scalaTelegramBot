import org.scalatest._
import PollDeps._
import PollDeps.Parsers.QuestionParser

class QuestionParserShould extends FlatSpec with Matchers {
  "QuestionParser" should "parse question in all positive cases" in {
    QuestionParser
      .parse(QuestionParser.definition, "(What color do you like ((for now))?)")
      .map(question => {
        info(question.asText)
        question.question shouldBe "What color do you like (for now)?"
        question.kind shouldBe Open
        question.options shouldBe None
      })
      .getOrElse(Failed) shouldBe Succeeded

    QuestionParser
      .parse(QuestionParser.definition, "(What color do you like ((for now))?) (open)")
      .map(question => {
        info(question.asText)
        question.question shouldBe "What color do you like (for now)?"
        question.kind shouldBe Open
        question.options shouldBe None
      })
      .getOrElse(Failed) shouldBe Succeeded

    QuestionParser
      .parse(QuestionParser.definition, "(What color do you like ((for now))?) (choice) (Blue) (Green) (Red)")
      .map(question => {
        info(question.asText)
        question.question shouldBe "What color do you like (for now)?"
        question.kind shouldBe Choice
        question.options.nonEmpty shouldBe true
        question.options.get shouldEqual List("Blue", "Green", "Red")
      })
      .getOrElse(Failed) shouldBe Succeeded

    QuestionParser
      .parse(QuestionParser.definition, "(What color do you like ((for now))?) (multi) (Blue) (Green) (Red)")
      .map(question => {
        info(question.asText)
        question.question shouldBe "What color do you like (for now)?"
        question.kind shouldBe Multi
        question.options.nonEmpty shouldBe true
        question.options.get shouldEqual List("Blue", "Green", "Red")
      })
      .getOrElse(Failed) shouldBe Succeeded
  }
}