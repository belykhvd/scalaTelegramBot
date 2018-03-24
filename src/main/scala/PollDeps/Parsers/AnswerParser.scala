package PollDeps.Parsers

import PollDeps.Answer

import scala.util.parsing.combinator._

object AnswerParser extends RegexParsers {
  def argument: Parser[String] = "\\(((\\(\\()|(\\)\\))|[^()])*\\)".r ^^ {
    arg => arg.substring(1, arg.length - 1).replace("((", "(").replace("))", ")")
  }
  def oneBasedIndex: Parser[Int] = "\\(\\d+\\)".r ^^ { str => str.substring(1, str.length - 1).toInt }
  def indexes: Parser[List[Int]] = rep1(oneBasedIndex)
  def stringAnswer: Parser[Answer] = oneBasedIndex ~ argument ^^ {
    case oneBasedIndex ~ answer => Answer(oneBasedIndex, stringAnswer = Some(answer))
  }
  def indexAnswers: Parser[Answer] = oneBasedIndex ~ indexes ^^ {
    case oneBasedIndex ~ indexes => Answer(oneBasedIndex, indexAnswers = Some(indexes))
  }
  def definition: Parser[Answer] = indexAnswers | stringAnswer
}