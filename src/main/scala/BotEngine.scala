package TelegramBot

import PollDeps.Contexts.{AnsweringContext, EditContext}
import PollDeps.Parsers.{AnswerParser, PollParser}
import PollDeps._

import scala.util._

object BotEngine {
  private var contexts = Map.empty[Int, Long]
  private var polls = Map.empty[Long, ContextDependentPoll]

  def create_poll(argString: String, userId: Int): String = {
    PollParser
      .parse(PollParser.definition, argString)
      .map(poll => {
        val contextDependentPoll = ContextDependentPoll(poll, userId, PollUUIDGenerator.next)
        polls += contextDependentPoll.pollId -> contextDependentPoll
        s"Created. Your poll id is ${contextDependentPoll.pollId}."
      }).getOrElse("Faulted: Check command syntax.")
  }

  def delete_poll(argString: String, userId: Int): String =
    Try(argString.toLong).map(pollId => {
      polls.get(pollId).map(poll => {
        if (poll.creatorId != userId) "Faulted: No permissions."
        else {
          polls = polls - pollId
          "Succeeded: Poll removed."
        }
      }).getOrElse("Faulted: No poll with such id.")
    }).getOrElse("Faulted: Check command syntax.")

  def list(userId: Int): String = {
    val builder = StringBuilder.newBuilder
    for (poll <- polls)
      builder.append(poll.toString).append("\n")
    builder.toString
  }

  def start_poll(argString: String, userId: Int): String =
    Try(argString.toLong).map(pollId => {
      polls.get(pollId).map(poll => {
        if (poll.creatorId != userId) "Faulted: Permission denied."
        else if (poll.wrappedPoll.status == Running) "Warning: Poll is already started."
        else {
          polls += pollId -> ContextDependentPoll(polls(pollId).wrappedPoll.started, userId, pollId)
          "Succeeded: Poll started."
        }
      }).getOrElse("Faulted: No poll with such id.")
    }).getOrElse("Faulted: Check command syntax.")

  def stop_poll(argString: String, userId: Int): String =
    Try(argString.toLong).map(pollId => {
      polls.get(pollId).map(poll => {
        if (poll.creatorId != userId) "Faulted: Permission denied."
        else if (poll.wrappedPoll.status == Running) "Warning: Poll is already started."
        else {
          polls += pollId -> ContextDependentPoll(polls(pollId).wrappedPoll.started, userId, pollId)
          "Succeeded: Poll stopped."
        }
      }).getOrElse("Faulted: No poll with such id.")
    }).getOrElse("Faulted: Check command syntax.")

  def result(argString: String, userId: Int): String =
    Try(argString.toLong).map(pollId => {
      polls.get(pollId).map(poll => {
        val wrappedPoll = poll.wrappedPoll
        if (wrappedPoll.visibility == Continuous) resultAsText(wrappedPoll)
        else "Faulted: You may see poll results only when poll stopped."
      }).getOrElse("Faulted: No poll with such id.")
    }).getOrElse("Faulted: Check command syntax.")

  private def resultAsText(poll: Poll): String = {
    val builder = StringBuilder.newBuilder
    for (i <- poll.questions.indices; question <- poll.questions) {
      val answers = poll.answers.getOrElse(i + 1, List.empty)
      builder.append(s"${i+1}. ${question.headerText} [${answers.size} answers]")
    }
    //s"[User](tg://user?id=${pair._1})"
    builder.toString
  }

  def begin(argString: String, userId: Int): String =
    Try(argString.toLong).map(pollId => {
      polls.get(pollId).map(poll => {
        if (poll.wrappedPoll.getStatus == Running ||
            poll.wrappedPoll.getStatus == NotStarted && poll.creatorId == userId) {
          contexts += (userId -> pollId)
          s"Succeeded: You start working with poll $pollId."
        } else "Faulted: Poll is not started and you have no edit permissions."
      }).getOrElse("Faulted: Poll doesn't exist.")
    }).getOrElse("Faulted: Check command syntax.")

  def add_question(argString: String, userId: Int): String =
    contexts.get(userId).map(pollId => {
      val poll = polls(pollId)
      if (poll.creatorId == userId && poll.wrappedPoll.getStatus == NotStarted)
        EditContext(poll, (k, v) => { polls += (k -> v) }).add_question(argString)
      else "Faulted: You have no permissions or poll is running or stopped."
    }).getOrElse("Faulted: Use begin <pollId> before applying operations.")

  def delete_question(argString: String, userId: Int): String =
    contexts.get(userId).map(pollId => {
      val poll = polls(pollId)
      if (poll.creatorId == userId && poll.wrappedPoll.getStatus == NotStarted)
        EditContext(poll, (k, v) => { polls += (k -> v) }).delete_question(argString)
      else "Faulted: You have no permissions or poll is running or stopped."
    }).getOrElse("Faulted: Use begin <pollId> before applying operations.")

  /*def answer(argString: String, userId: Int): String =
    contexts.get(userId).map(pollId => {
      val wrappedPoll = polls(pollId).wrappedPoll
      AnswerParser
        .parse(AnswerParser.definition, argString)
        .map(answer => {
          val questionNumber = answer.questionOneBasedIndex
          val questionAnswers = wrappedPoll.answers.getOrElse(questionNumber, List.empty[(Int, Answer)])
          if (questionAnswers.isEmpty) {
            wrappedPoll.answers += questionNumber -> List((userId, answer))
            s"Succeeded: Answer for question number $questionNumber recorded."
          }
          else if (!questionAnswers.map(pair => pair._1).contains(userId)) {
            wrappedPoll.answers += questionNumber -> (wrappedPoll.answers(questionNumber) :+ (userId, answer))
            s"Succeeded: Answer for question number $questionNumber recorded."
          } else "Faulted: You've already answered that question."
        }).getOrElse("Faulted: Check command syntax.")
    }).getOrElse("Faulted: Use begin <pollId> before applying operations.")*/

  def answer(argString: String, userId: Int): String =
    contexts.get(userId).map(pollId => {
      val wrappedPoll = polls(pollId).wrappedPoll
      new AnsweringContext(wrappedPoll, userId).answer(argString)
    }).getOrElse("Faulted: Use begin <pollId> before applying operations.")

  def view(userId: Int): String =
    contexts.get(userId).map(pollId => {
      val wrappedPoll = polls(pollId).wrappedPoll
      wrappedPoll.asText
    }).getOrElse("Faulted: Use begin <pollId> before applying operations.")

  def end(userId: Int): String =
    contexts.get(userId).map(pollId => {
      contexts -= userId
      s"Succeeded: You finished working with poll $pollId."
    }).getOrElse("Faulted: Use begin <pollId> before applying operations.")
}