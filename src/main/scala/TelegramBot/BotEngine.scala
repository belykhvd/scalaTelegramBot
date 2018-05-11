package TelegramBot

import TelegramBot.Parsers.{AnswerParser, PollParser, QuestionParser}
import org.joda.time.DateTime

import scala.util._

object BotEngine {
  var state: State = State(Map.empty, Map.empty)

  def createPoll(argString: String, user: Int): String =
    PollParser.parse(PollParser.definition, argString).map(poll => {
      val pollId = PollUUIDGenerator.next
      state = state.copy(polls = state.polls + (pollId -> poll.copy(id = Some(pollId), creator = Some(user))))
      ResponseConstants.pollCreated(pollId)
    }).getOrElse(ErrorConstants.badSyntax)

  def deletePoll(argString: String, user: Int): String =
    Try(argString.toLong).map(pollId => {
      state.polls.get(pollId).map(poll => {
        if (poll.creator.getOrElse(-1) == user) {
          state = state.copy(polls = state.polls - pollId)
          ResponseConstants.pollDeleted(pollId)
        } else ErrorConstants.noPermissions
      }).getOrElse(ErrorConstants.noPoll)
    }).getOrElse(ErrorConstants.badSyntax)

  def list(userId: Int): String =
    state.polls.map(t => t._2.toString).mkString("\n")

  def startPoll(argString: String, user: Int): String =
    Try(argString.toLong).map(pollId => {
      state.polls.get(pollId).map(poll => {
        if (poll.creator.getOrElse(-1) != user) ErrorConstants.noPermissions
        else if (poll.status(DateTime.now) == Running) "Warning: Poll is already started."
        else {
          state = state.copy(polls = state.polls + (pollId -> poll.started))
          ResponseConstants.pollStarted(pollId)
        }
      }).getOrElse(ErrorConstants.noPoll)
    }).getOrElse(ErrorConstants.badSyntax)

  def stopPoll(argString: String, user: Int): String =
    Try(argString.toLong).map(pollId => {
      state.polls.get(pollId).map(poll => {
        if (poll.creator.getOrElse(-1) != user) ErrorConstants.noPermissions
        else if (poll.status(DateTime.now) == NotStarted) "Warning: Poll is not started."
        else if (poll.status(DateTime.now) == Stopped) "Warning: Poll is already stopped."
        else {
          state = state.copy(polls = state.polls + (pollId -> poll.stopped))
          ResponseConstants.pollStopped(pollId)
        }
      }).getOrElse(ErrorConstants.noPoll)
    }).getOrElse(ErrorConstants.badSyntax)

  def result(argString: String, user: Int): String =
    Try(argString.toLong).map(pollId => {
      state.polls.get(pollId).map(poll => {
        if (poll.visibility == Continuous || poll.status(DateTime.now) == Stopped) resultAsText(poll)
        else ErrorConstants.forbiddenViewResults
      }).getOrElse(ErrorConstants.noPoll)
    }).getOrElse(ErrorConstants.badSyntax)

  private def resultAsText(poll: Poll): String =
    poll.questions.zipWithIndex.map {
      case (question, i) =>
        val header = s"${i + 1}. ${question.question} [${question.kind}]"
        header + question.options.getOrElse(question.answers.keys).zipWithIndex.map {
          case (option, optionIndex) =>
            val optionText = s"\n\t(${optionIndex + 1}) $option - ${question.answers(option)._1} voted"
            val users = question.answers(option)._2.mkString(", ")
            if (users.nonEmpty) s"$optionText by $users"
            else optionText
        }.mkString("")
      }.mkString("\n")

  def begin(argString: String, user: Int): String =
    Try(argString.toLong).map(pollId => {
      state.polls.get(pollId).map(poll => {
        val now = DateTime.now
        if (poll.status(now) == Running ||
            poll.status(now) == NotStarted && poll.creator.getOrElse(-1) == user) {
          state = state.copy(contexts = state.contexts + (user -> pollId))
          ResponseConstants.begin(pollId)
        } else ErrorConstants.unableUpdatePoll
      }).getOrElse(ErrorConstants.noPoll)
    }).getOrElse(ErrorConstants.badSyntax)

  def addQuestion(argString: String, user: Int): String =
    QuestionParser.parse(QuestionParser.definition, argString).map(question => {
      state.contexts.get(user).map(pollId => {
        state.polls.get(pollId).map(poll => {
          if (canUpdatePoll(poll, user)) {
            state = state.copy(polls = state.polls + (poll.id.get -> poll.withQuestion(question)))
            ResponseConstants.addedQuestion
          } else ErrorConstants.unableUpdatePoll
        }).getOrElse(ErrorConstants.fatalKeyGetLost)
      }).getOrElse(ErrorConstants.noContext)
    }).getOrElse(ErrorConstants.badSyntax)

  def deleteQuestion(argString: String, user: Int): String =
    Try(argString.toInt).map(number => {
      state.contexts.get(user).map(pollId => {
        state.polls.get(pollId).map(poll => {
          if (canUpdatePoll(poll, user))
            poll.withoutQuestion(number).fold(message => message, updatedPoll => {
              state = state.copy(polls = state.polls + (pollId -> updatedPoll))
              ResponseConstants.deletedQuestion
            }) else ErrorConstants.unableUpdatePoll
        }).getOrElse(ErrorConstants.fatalKeyGetLost)
      }).getOrElse(ErrorConstants.noContext)
    }).getOrElse(ErrorConstants.badSyntax)

  private def canUpdatePoll(poll: Poll, user: Int) =
    poll.creator.getOrElse(-1) == user && poll.status(DateTime.now) == NotStarted

  def answer(argString: String, user: Int): String =
    AnswerParser.parse(AnswerParser.definition, argString).map(answer => {
      state.contexts.get(user).map(pollId => {
        state.polls.get(pollId).map(poll => {
          if (poll.status(DateTime.now) == Running)
            poll.withAnswer(answer, user).fold(message => message, updatedPoll => {
              state = state.copy(polls = state.polls + (pollId -> updatedPoll))
              ResponseConstants.answerRecorded
            }) else ErrorConstants.unableUpdatePoll
        }).getOrElse(ErrorConstants.fatalKeyGetLost)
      }).getOrElse(ErrorConstants.noContext)
    }).getOrElse(ErrorConstants.badSyntax)

  def view(user: Int): String =
    state.contexts.get(user).map(pollId => {
      state.polls.get(pollId).map(poll => {
        poll.asText
      }).getOrElse(ErrorConstants.fatalKeyGetLost)
    }).getOrElse(ErrorConstants.noContext)

  def end(user: Int): String =
    state.contexts.get(user).map(pollId => {
      state = state.copy(contexts = state.contexts - user)
      ResponseConstants.end(pollId)
    }).getOrElse(ErrorConstants.noContext)
}