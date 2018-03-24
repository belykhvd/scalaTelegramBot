package PollDeps.Contexts

import PollDeps.{Answer, ContextDependentPoll}
import PollDeps.Parsers.AnswerParser
import PollDeps._

import scala.util._

class AnsweringContext(poll: Poll, userId: Int) {
  def answer(argString: String): String = {
    AnswerParser
      .parse(AnswerParser.definition, argString)
      .map(answer => {
        val questionNumber = answer.questionOneBasedIndex
        if (!isConsistent(poll, answer, poll.questions(questionNumber - 1)))
          s"Faulted: Answer is mal-formed for question $questionNumber"
        else {
          val questionAnswers = poll.answers.getOrElse(questionNumber, List.empty[(Int, Answer)])
          if (questionAnswers.isEmpty) {
            poll.answers += questionNumber -> List((userId, answer))
            s"Succeeded: Answer for question number $questionNumber recorded."
          }
          else if (!questionAnswers.map(pair => pair._1).contains(userId)) {
            poll.answers += questionNumber -> (poll.answers(questionNumber) :+ (userId, answer))
            s"Succeeded: Answer for question number $questionNumber recorded."
          } else "Faulted: You've already answered that question."
        }
      }).getOrElse("Faulted: Check command syntax.")
  }

  private def isConsistent(poll: Poll, answer: Answer, question: Question): Boolean = {
    val isAnswerConsistent = answer.stringAnswer.nonEmpty && answer.indexAnswers.isEmpty ||
      answer.stringAnswer.isEmpty && answer.indexAnswers.nonEmpty &&
        answer.indexAnswers.map(list => list.size).getOrElse(0) > 0
    val isAnswerQuestionRelated = answer.stringAnswer.nonEmpty && question.kind == Open ||
      answer.indexAnswers.nonEmpty && question.kind != Open
    val isCorrectNumber = answer.questionOneBasedIndex <= poll.questions.size
    isAnswerConsistent && isAnswerQuestionRelated
  }
}

object AnsweringContext {
  def apply(poll: ContextDependentPoll, pollUpdater: (Long, ContextDependentPoll) => Unit) =
    new EditContext(poll, pollUpdater)
}