package PollDeps.Parsers

import PollDeps.{AfterStop, Continuous, Poll, ResultsVisibility}
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat

import scala.util.parsing.combinator._

object PollParser extends RegexParsers {
  def argument: Parser[String] = raw"\(((\(\()|(\)\))|[^()])*\)".r ^^ {
    arg => arg.substring(1, arg.length - 1).replace("((", "(").replace("))", ")")
  }
  def anonymity: Parser[Boolean] = ("(yes)" | "(no)") ^^ {
    case "(yes)" => true
    case "(no)" => false
  }
  def visibility: Parser[ResultsVisibility] = ("(afterstop)" | "(continuous)") ^^ {
    case "(afterstop)" => AfterStop
    case "(continuous)" => Continuous
  }
  def time: Parser[DateTime] = argument ^^ {
    argument => DateTime.parse(argument, DateTimeFormat.forPattern("HH:mm:ss yy:MM:dd"))
  }
  def name: Parser[Poll] = argument ^^ { name => Poll(name) }
  def toAnonymity: Parser[Poll] = argument ~ anonymity ^^ { case name ~ anonymity => Poll(name, anonymity) }
  def toVisibility: Parser[Poll] = argument ~ anonymity ~ visibility ^^ {
    case name ~ anonymity ~ visibility => Poll(name, anonymity, visibility)
  }
  def toStartTime: Parser[Poll] = argument ~ anonymity ~ visibility ~ time ^^ {
    case name ~ anonymity ~ visibility ~ startTime
      => Poll(name, anonymity, visibility, Some(startTime))
  }
  def toEndTime: Parser[Poll] = argument ~ anonymity ~ visibility ~ time ~ time ^^ {
    case name ~ anonymity ~ visibility ~ startTime ~ endTime
      => Poll(name, anonymity, visibility, Some(startTime), Some(endTime))
  }
  def definition: Parser[Poll] = toEndTime | toStartTime | toVisibility | toAnonymity | name
}
