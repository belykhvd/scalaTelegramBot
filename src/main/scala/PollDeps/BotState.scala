package PollDeps


/*
* contexts: userID -> pollID
* polls: pollID -> poll
*/
case class BotState(contexts: Map[Int, Long], polls: Map[Long, Poll]) {

}