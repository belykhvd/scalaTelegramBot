package TelegramBot.Parsers

import TelegramBot._
import scala.util.parsing.combinator.RegexParsers

object QuestionParser extends RegexParsers {
  def argument: Parser[String] = "\\(((\\(\\()|(\\)\\))|[^()])*\\)".r ^^ {
    arg => arg.substring(1, arg.length - 1).replace("((", "(").replace("))", ")")
  }
  def kind: Parser[QuestionKind] = ("(open)" | "(choice)" | "(multi)") ^^ {
    case "(open)" => Open
    case "(choice)" => Choice
    case "(multi)" => Multi
  }
  def options: Parser[List[String]] = rep(argument)

  def openQuestion: Parser[Question] = argument ^^ { argument => Question(argument, Open, Map.empty) }
  def optionsQuestion: Parser[Question] = (argument ~ kind ~ options) ^^ {
    case argument ~ kind ~ options =>
      Question(argument, kind, options.map(option => option -> (0, List.empty)).toMap,
        if (options.nonEmpty) Some(options) else None)
  }

  def definition: Parser[Question] = optionsQuestion | openQuestion
}