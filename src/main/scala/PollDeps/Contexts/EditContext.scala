package PollDeps.Contexts

import PollDeps.ContextDependentPoll
import PollDeps.Parsers.QuestionParser

import scala.util._

class EditContext(contextPoll: ContextDependentPoll, pollUpdater: (Long, ContextDependentPoll) => Unit) {
  def add_question(argString: String): String =
    QuestionParser
      .parse(QuestionParser.definition, argString)
      .map(question => {
        pollUpdater(contextPoll.pollId,
          ContextDependentPoll(contextPoll.wrappedPoll.withQuestion(question), contextPoll.creatorId, contextPoll.pollId))
        s"Succeeded: Added ${question.kind} question" +
          s" with ${question.options.map(list => list.size).getOrElse(0)} options."
      }).getOrElse("Faulted: Check command syntax.")

  def delete_question(argString: String): String =
    Try(argString.toInt).map(n => {
      if (n >= 1 && n <= contextPoll.wrappedPoll.questions.size) {
        pollUpdater(contextPoll.pollId,
          ContextDependentPoll(contextPoll.wrappedPoll.withoutQuestion(n), contextPoll.creatorId, contextPoll.pollId))
        s"Succeeded: Question number $n deleted."
      } else "Faulted: Question number is incorrect."
    }).getOrElse("Faulted: Check command syntax.")
}

object EditContext {
  def apply(poll: ContextDependentPoll, pollUpdater: (Long, ContextDependentPoll) => Unit) =
    new EditContext(poll, pollUpdater)
}