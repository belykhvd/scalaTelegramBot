import TelegramBot._
import org.scalatest._
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat

class FunctionalTest extends FlatSpec with Matchers {
  "Engine" should "pass scenario" in {
    val engine = BotEngine
    val command = "(NEW POLL ((TEST VERSION))) (no) (afterstop) (11:11:11 19:11:11) (12:12:12 19:12:12)"
    val user = 123

    val createMessage = engine.createPoll(command, user)
    info(createMessage)

    val pollId = createMessage.split(' ')(2).toLong
    val pollIdString = pollId.toString
    var poll = engine.state.polls(pollId)
    poll.creator.getOrElse(-1) shouldBe user
    poll.name shouldBe "NEW POLL (TEST VERSION)"
    poll.anonymous shouldBe false
    poll.visibility shouldBe AfterStop
    poll.startTime.getOrElse(None) shouldBe parsedDate("11:11:11 19:11:11")
    poll.endTime.getOrElse(None) shouldBe parsedDate("12:12:12 19:12:12")

    info(engine.begin(pollIdString, user))

    info(engine.view(user))

    info(engine.addQuestion("(QUESTION FIRST) (choice)\n(Good)\n(Better)\n(The Best)", user))
    poll = engine.state.polls(pollId)
    poll.questions.length shouldBe 1

    info(engine.addQuestion("(QUESTION SECOND) (open)", user))
    poll = engine.state.polls(pollId)
    poll.questions.length shouldBe 2

    info(engine.addQuestion("(QUESTION THIRD) (multi)\n(Variant 1)\n(Variant 2)", user))
    poll = engine.state.polls(pollId)
    poll.questions.length shouldBe 3

    info(engine.view(user))

    info(engine.addQuestion("(QUESTION TO DELETE) (multi)\n(First)\n(Second)\n(Third)", user))
    poll = engine.state.polls(pollId)
    poll.questions.length shouldBe 4
    info(engine.deleteQuestion("4", user))
    poll = engine.state.polls(pollId)
    poll.questions.length shouldBe 3

    info(engine.startPoll(pollIdString, user))
    poll = engine.state.polls(pollId)
    poll.status(DateTime.now) shouldBe Running

    info(engine.answer("(1) (2)", user))
    poll = engine.state.polls(pollId)
    val question = poll.questions.head
    val answer = question.answers(question.options.getOrElse(List.empty)(1))
    answer._1 shouldBe 1
    answer._2 shouldBe List(123)

    info(engine.answer("(2) (My answer is 42.)", user))

    info(engine.end(user))

    info(engine.stopPoll(pollIdString, user))
    poll = engine.state.polls(pollId)
    poll.status(DateTime.now) shouldBe Stopped

    info(engine.result(pollIdString, user))

    info(engine.deletePoll(pollIdString, user))
    engine.state.polls.getOrElse(pollId, None) shouldBe None
  }

  def parsedDate(dateTime: String): DateTime = DateTime.parse(dateTime, DateTimeFormat.forPattern("HH:mm:ss yy:MM:dd"))
}