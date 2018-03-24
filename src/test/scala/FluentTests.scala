/*import Fluent._
import PollDeps.{AfterStop, Continuous, Running, Stopped}
import PollDeps.{Continuous, Running, Stopped}
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import org.scalatest._

import scala.util.Try

class FluentTests extends FlatSpec with Matchers {
  def fixture = new {
    val userId = 123
    val poll = new PollManager
    val context = new ContextManager
    val commonContext = new CommonContext(userId, poll, context)
  }

  "PollManager" should "return poll with correct properties" in {
    val f = fixture
    val createEither = f.poll.createdBy(123)
      .withName(Try("BreakfastIssue")).withAnonymity(Try("yes")).withVisibility(Try("continuous"))
      .startsAt(Try("14:15:16 18:03:05"))
      .endsAt(Try("16:17:18 18:03:06")).create

    createEither.isRight shouldBe true
    val pollId = createEither.right.get

    val getEither = f.poll.withId(pollId).get
    getEither.isRight shouldBe true

    val createdPoll = getEither.right.get
    createdPoll.creatorId shouldBe 123
    createdPoll.id shouldBe pollId
    createdPoll.name shouldBe "BreakfastIssue"
    createdPoll.isAnonymous shouldBe true
    createdPoll.visibility shouldBe Continuous
    createdPoll.startTime.nonEmpty shouldBe true
    createdPoll.startTime.get shouldBe DateTime.parse("14:15:16 18:03:05", DateTimeFormat.forPattern("HH:mm:ss yy:MM:dd"))
    createdPoll.endTime.nonEmpty shouldBe true
    createdPoll.endTime.get shouldBe DateTime.parse("16:17:18 18:03:06", DateTimeFormat.forPattern("HH:mm:ss yy:MM:dd"))
  }

  "CommonContext" should "create poll from parsed arguments" in {
    val f = fixture
    val createEither = f.commonContext.create_poll("(BreakfastIssue) (no) (afterstop) (14:15:16 18:03:05) (16:17:18 18:03:06)")
    createEither.isRight shouldBe true

    val pollId = createEither.right.get
    val getEither = f.poll.withId(pollId).get
    getEither.isRight shouldBe true

    val createdPoll = getEither.right.get
    createdPoll.creatorId shouldBe 123
    createdPoll.id shouldBe pollId
    createdPoll.name shouldBe "BreakfastIssue"
    createdPoll.isAnonymous shouldBe false
    createdPoll.visibility shouldBe AfterStop
    createdPoll.startTime.nonEmpty shouldBe true
    createdPoll.startTime.get shouldBe DateTime.parse("14:15:16 18:03:05", DateTimeFormat.forPattern("HH:mm:ss yy:MM:dd"))
    createdPoll.endTime.nonEmpty shouldBe true
    createdPoll.endTime.get shouldBe DateTime.parse("16:17:18 18:03:06", DateTimeFormat.forPattern("HH:mm:ss yy:MM:dd"))
  }

  "CommonContext" should "remove poll that exists" in {
    val f = fixture
    val createEither = f.commonContext.create_poll("(BreakfastIssue) (no) (afterstop) (14:15:16 18:03:05) (16:17:18 18:03:06)")
    val pollId = createEither.right.get

    val successMessage = f.commonContext.delete_poll(s"(${createEither.right.get.toString})")
    successMessage shouldBe s"Poll with id $pollId removed"

    val errorMessage = f.commonContext.delete_poll("(1234)")
    errorMessage shouldBe s"Faulted: Poll with id 1234 doesn't exist"
  }

  "List command" should "just work" in {
    val f = fixture
    f.poll.createdBy(123).withName(Try("JUST WHY?")).withAnonymity(Try("yes")).create
    f.poll.createdBy(234).withName(Try("ANOTHER WELL FORMED POLL")).withVisibility(Try("afterstop")).create
    info(f.commonContext.list)
  }

  "Start/stop methods" should "switch status property" in {
    val f = fixture
    val pollId = f.poll.createdBy(123).withName(Try("JUST WHY?")).withAnonymity(Try("yes")).create.right.get
    f.commonContext.start_poll(s"($pollId)")
    f.poll.withId(pollId).get.right.get.status shouldBe Running
    f.commonContext.stop_poll(s"($pollId)")
    f.poll.withId(pollId).get.right.get.status shouldBe Stopped
  }

  /*"EditorContext" should "add questions with three types" in {
    val f = fixture
    val pollId = f.poll.createdBy(123).withName(Try("JUST WHY?")).withAnonymity(Try("yes")).create.right.get
    val editorContext = new EditorContext(f.userId, pollId, f.poll, f.context)
    editorContext.add_question("(Why the sky is blue???)")
    editorContext.add_question("(Why the sky is black???) (open)")
    editorContext.add_question("(Who will be the first???) (choice) (A) (B) (C)")
    editorContext.add_question("(Who will be the first (again)???) (multi) (A) (B) (C)")

    val pollContent = f.poll.withId(pollId).get.right.get.pollContent
    pollContent.size shouldBe 4
    pollContent.head.isInstanceOf[OpenQuestion] shouldBe true
    pollContent(1).isInstanceOf[OpenQuestion] shouldBe true
    pollContent(2).isInstanceOf[ChoiceQuestion] shouldBe true
    pollContent(3).isInstanceOf[MultiQuestion] shouldBe true

    pollContent.head.asInstanceOf[OpenQuestion].question shouldBe "Why the sky is blue???"
    pollContent(3).asInstanceOf[MultiQuestion].variants shouldBe "A B C".split(" ")
  }

  "Question" shouldBe "created correct with"*/
}*/