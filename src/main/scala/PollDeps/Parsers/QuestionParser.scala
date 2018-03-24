package PollDeps.Parsers

import PollDeps._
import scala.util.parsing.combinator.RegexParsers

object QuestionParser extends RegexParsers {
  def argument: Parser[String] = raw"\(((\(\()|(\)\))|[^()])*\)".r ^^ {
    arg => arg.substring(1, arg.length - 1).replace("((", "(").replace("))", ")")
  }
  def kind: Parser[QuestionKind] = ("(open)" | "(choice)" | "(multi)") ^^ {
    case "(open)" => Open
    case "(choice)" => Choice
    case "(multi)" => Multi
  }
  def options: Parser[List[String]] = rep(argument)
  def openQuestion: Parser[Question] = argument ^^ { argument => Question(argument, Open) }
  def optionsQuestion: Parser[Question] = (argument ~ kind ~ options) ^^ {
    case argument ~ kind ~ options => Question(argument, kind, if (options.nonEmpty) Some(options) else None)
  }

  def definition: Parser[Question] = optionsQuestion | openQuestion
}