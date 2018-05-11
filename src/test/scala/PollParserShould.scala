import TelegramBot.Parsers.PollParser
import TelegramBot.{AfterStop, Continuous}
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import org.scalatest._

class PollParserShould extends FlatSpec with Matchers {
  "PollParser" should "parse all positive cases" in {
    PollParser
      .parse(PollParser.definition, "(Where to go tomorrow ((friday))?)")
      .map(poll => poll.name shouldBe "Where to go tomorrow (friday)?")
      .getOrElse(Failed) shouldBe Succeeded

    PollParser
      .parse(PollParser.definition, "(Where to go tomorrow ((friday))?) (yes)")
      .map(poll => poll.anonymous shouldBe true)
      .getOrElse(Failed) shouldBe Succeeded

    PollParser
      .parse(PollParser.definition, "(Where to go tomorrow ((friday))?) (no) (afterstop)")
      .map(poll => {
        poll.anonymous shouldBe false
        poll.visibility shouldBe AfterStop
      }).getOrElse(Failed) shouldBe Succeeded

    PollParser
      .parse(PollParser.definition, "(Where to go tomorrow ((friday))?) (yes) (continuous) (12:52:17 18:03:14)")
      .map(poll => {
        poll.visibility shouldBe Continuous
        poll.startTime.get shouldBe DateTime.parse(
          "12:52:17 18:03:14", DateTimeFormat.forPattern("HH:mm:ss yy:MM:dd"))
      }).getOrElse(Failed) shouldBe Succeeded

    PollParser
      .parse(PollParser.definition,
        "(Where to go tomorrow ((friday))?) (yes) (afterstop) (12:52:17 18:03:14) (12:52:17 18:03:14)")
      .map(poll => {
        poll.startTime.get shouldBe DateTime.parse(
          "12:52:17 18:03:14", DateTimeFormat.forPattern("HH:mm:ss yy:MM:dd"))
        poll.endTime.get shouldBe DateTime.parse(
          "12:52:17 18:03:14", DateTimeFormat.forPattern("HH:mm:ss yy:MM:dd"))
      }).getOrElse(Failed) shouldBe Succeeded
  }
}