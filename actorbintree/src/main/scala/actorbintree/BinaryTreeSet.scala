/**
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */
package actorbintree

import actorbintree.BinaryTreeNode.{CopyFinished, CopyTo}
import actorbintree.BinaryTreeSet._
import akka.actor._

import scala.collection.immutable.Queue

object BinaryTreeSet {

  trait Operation {
    def requester: ActorRef
    def id: Int
    def elem: Int
  }

  trait OperationReply {
    def id: Int
  }

  /** Request with identifier `id` to insert an element `elem` into the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Insert(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to check whether an element `elem` is present
    * in the tree. The actor at reference `requester` should be notified when
    * this operation is completed.
    */
  case class Contains(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to remove the element `elem` from the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Remove(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request to perform garbage collection*/
  case object GC

  /** Holds the answer to the Contains request with identifier `id`.
    * `result` is true if and only if the element is present in the tree.
    */
  case class ContainsResult(id: Int, result: Boolean) extends OperationReply
  
  /** Message to signal successful completion of an insert or remove operation. */
  case class OperationFinished(id: Int) extends OperationReply

}


class BinaryTreeSet extends Actor with ActorLogging{
  import BinaryTreeSet._

  def createRoot: ActorRef = context.actorOf(BinaryTreeNode.props(0, initiallyRemoved = true))

  var root = createRoot

  // optional
  var pendingQueue = Queue.empty[Operation]

  // optional
  def receive = normal

  // optional
  /** Accepts `Operation` and `GC` messages. */
  val normal: Receive = {
    case o: Operation =>
      root.forward(o)
    case GC =>
      val newRoot = createRoot
      root ! CopyTo(newRoot)
      log.info(s"Becoming garbageCollection($newRoot)")
      context.become(garbageCollecting(newRoot))
    case a: Any => log.warning("[normal] Got unexpected {}", a)
  }

  // optional
  /** Handles messages while garbage collection is performed.
    * `newRoot` is the root of the new binary tree where we want to copy
    * all non-removed elements into.
    */
  def garbageCollecting(newRoot: ActorRef): Receive = {
    case o: Operation =>
      log.info("Queuing {}", o)
      pendingQueue = pendingQueue :+ o
    case CopyFinished =>
      log.info("CopyFinished received")
      context.stop(root)
      root = newRoot
      pendingQueue.foreach(operation => {
        log.info("Resending {}", operation)
        self ! operation
      })
      pendingQueue = Queue()
      log.info("Becoming normal")
      context.become(normal)
    case a: Any => log.warning(s"[garbageCollecting] Ünexpected $a")

  }

  def replayQueue(): Receive = {
    case o: Operation =>
      root ! o
      if (pendingQueue.isEmpty) {
        context.become(normal)
      } else {
        self ! pendingQueue.head
        pendingQueue = pendingQueue.tail
      }
  }

}

object BinaryTreeNode {
  trait Position

  case object Left extends Position
  case object Right extends Position

  case class CopyTo(treeNode: ActorRef)
  case object CopyFinished

  def props(elem: Int, initiallyRemoved: Boolean) =
    Props(classOf[BinaryTreeNode],  elem, initiallyRemoved)
}

class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor with ActorLogging{
  import BinaryTreeNode._

  var subtrees = Map[Position, ActorRef]()
  var removed = initiallyRemoved

  override def receive: Receive = normal
  def insert(ins: Insert): Unit = {
    if (elem == ins.elem) {
      removed = false
      ins.requester ! OperationFinished(ins.id)
    } else if (ins.elem < elem) {
      if (!subtrees.isDefinedAt(Left)) {
        subtrees = subtrees.updated(
          Left,
          context.actorOf(props(ins.elem, false), "node-" + ins.elem)
        )
        ins.requester ! OperationFinished(ins.id)
      } else
        subtrees(Left) forward ins
    } else {
      if (!subtrees.isDefinedAt(Right)) {
        subtrees  = subtrees.updated(
          Right,
          context.actorOf(props(ins.elem, false), "node-" + ins.elem)
        )
        ins.requester ! OperationFinished(ins.id)
      } else
        subtrees(Right) forward ins
    }
  }
  def contains(c: Contains) = {
    if (c.elem == elem) {
      c.requester ! ContainsResult(c.id, !removed)
    } else if (c.elem < elem && subtrees.isDefinedAt(Left)) {
      subtrees(Left) forward c
    } else if (c.elem > elem && subtrees.isDefinedAt(Right)) {
      subtrees(Right) forward c
    } else {
      c.requester ! ContainsResult(c.id, false)
    }
  }

  def remove(r: Remove) = {
    if (elem == r.elem) {
      removed = true
      r.requester ! OperationFinished(r.id)
    } else if (r.elem < elem && subtrees.isDefinedAt(Left)) {
        subtrees(Left).forward(r)
    } else if (r.elem > elem && subtrees.isDefinedAt(Right)) {
      subtrees(Right).forward(r)
    } else
      r.requester ! OperationFinished(r.id)
  }
  // optional
  /** Handles `Operation` messages and `CopyTo` requests. */
  val normal: Receive = {
    case ins: Insert => insert(ins)
    case c: Contains => contains(c)
    case r: Remove => remove(r)
    case copy: CopyTo =>
      if (!removed)
        copy.treeNode ! Insert(self, 0, elem)
      val opSet = subtrees.values.toSet
      for (a <- opSet) {
        a ! CopyTo(copy.treeNode)
      }
      if (opSet.isEmpty && removed) {
        context.parent ! CopyFinished
      } else {
        val whatToBecome: Actor.Receive = copying(opSet, removed)
        log.info(s"Becoming copying($opSet, $removed)")
        context.become(whatToBecome)
      }
    case idunno: Any => log.warning("Received unexpected {}", idunno)
  }

  // optional
  /** `expected` is the set of ActorRefs whose replies we are waiting for,
    * `insertConfirmed` tracks whether the copy of this node to the new tree has been confirmed.
    */
  def copying(expected: Set[ActorRef], insertConfirmed: Boolean): Receive = {
    case OperationFinished(id) =>
      if (expected.isEmpty) {
        context.parent ! CopyFinished
        log.info(s"Becoming normal")
        context.become(normal)
      } else {
        val whatToBecome: Actor.Receive = copying(expected, true)
        log.info(s"Becoming copying($expected, true)")
        context.become(whatToBecome)
      }
    case CopyFinished =>
      val e2 = expected match {
        case _ if expected.isEmpty => expected
        case _ => expected.tail
      }
      if (e2.isEmpty && (insertConfirmed || removed)) {
        context.parent ! CopyFinished
        log.info(s"Becoming normal")
        context.become(normal)
      } else {
        val whatToBecome: Actor.Receive = copying(e2, insertConfirmed)
        log.info(s"Becoming copying($e2, $insertConfirmed)")
        context.become(whatToBecome)
      }
    case o: Operation =>
      context.parent forward o
    case a: Any => log.warning(s"Received unexpected $a")
  }


}
