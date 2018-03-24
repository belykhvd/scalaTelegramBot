package PollDeps

class ContextDependentPoll(val wrappedPoll: Poll, val creatorId: Long, val pollId: Long) {
  override def toString: String =
    s"Poll id: [$pollId] creatorId: [$creatorId] " + wrappedPoll.toString
}

object ContextDependentPoll {
    def apply(poll: Poll, creatorId: Long, pollId: Long) =
      new ContextDependentPoll(poll, creatorId, pollId)
}