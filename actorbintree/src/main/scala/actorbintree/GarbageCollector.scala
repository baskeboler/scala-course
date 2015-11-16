package actorbintree

import akka.actor.{ActorRef, Actor, ActorLogging}

/**
 * Creado por Victor Gil<baskeboler@gmail.com>, 13/11/15.
 */
class GarbageCollector extends Actor with ActorLogging {

  def normal: Receive = ???

  override def receive: Receive = normal
}

object GarbageCollector {
  case class StartGC(tree: ActorRef);
  case class GCFinished(result: ActorRef);
}
