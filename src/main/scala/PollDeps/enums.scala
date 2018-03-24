package PollDeps

sealed trait ResultsVisibility
case object AfterStop extends ResultsVisibility
case object Continuous extends ResultsVisibility

sealed trait PollStatus
case object NotStarted extends PollStatus
case object Running extends PollStatus
case object Stopped extends PollStatus

sealed trait QuestionKind
case object Open extends QuestionKind
case object Choice extends QuestionKind
case object Multi extends QuestionKind