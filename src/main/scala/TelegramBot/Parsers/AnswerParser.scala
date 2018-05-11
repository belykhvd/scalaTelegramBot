package TelegramBot.Parsers

import TelegramBot.Answer
import scala.util.parsing.combinator._

object AnswerParser extends RegexParsers {
  def argument: Parser[String] = "\\(((\\(\\()|(\\)\\))|[^()])*\\)".r ^^ {
    arg => arg.substring(1, arg.length - 1).replace("((", "(").replace("))", ")")
  }

  def number: Parser[Int] = "\\(\\d+\\)".r ^^ { str => str.substring(1, str.length - 1).toInt }
  def options: Parser[List[String]] = rep1(argument)

  def definition: Parser[Answer] = number ~ options ^^ { case number ~ options => Answer(number, options) }
}