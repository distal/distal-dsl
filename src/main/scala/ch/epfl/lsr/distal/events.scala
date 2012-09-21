package ch.epfl.lsr.distal

import ch.epfl.lsr.netty.protocol._
import reflect.{ClassTag}

import scala.collection.{ Set, Map }


/**
 * IMPORTANT: only execute one event per DSL runtime/protocol at a time. 
 */

trait DSLforEvents { 
  this :DSL =>

    final def senders : Set[ProtocolLocation] = __dslRuntime.senders.getOrElse { throw new Exception("senders is currently not defined") }
    final def sender :ProtocolLocation =  __dslRuntime.sender.getOrElse { throw new Exception("senders is currently not defined") }

}

trait DSLRuntimeForEvents { 
  var senders : Option[Set[ProtocolLocation]] = None
  var sender : Option[ProtocolLocation] = None
  var thread : Thread = null

  def setSender(loc :ProtocolLocation) = { 
    if(sender.nonEmpty)
      throw new Exception("sender already set "+thread+" (me = "+Thread.currentThread+")")
    else 
      sender = Some(loc)
    thread = Thread.currentThread
  }
  def resetSender = { 
    sender = None
  }

  def setSenders(locs :Set[ProtocolLocation]) = { 
    if(senders.nonEmpty)
      throw new Exception("senders already set")
    else 
      senders = Some(locs)
    thread = Thread.currentThread
  }
  def resetSenders = { 
    senders = None
  }

}


trait TriggeredEvent[T <: Message] { 
  def guard(m :T, senderLocation :ProtocolLocation) :Boolean

  def action(m :T, senderLocation :ProtocolLocation, dsl: DSLRuntimeForEvents) : Unit
}

abstract class MessageEvent[T <: Message](implicit tag :ClassTag[T]) extends TriggeredEvent[T] {  
  var sender :ProtocolLocation = null

  def checkAndExecute(m :Message, senderLocation :ProtocolLocation, dsl: DSLRuntimeForEvents) { 
    cast(m).foreach { t =>
      if(guard(t, senderLocation))
	action(t, senderLocation, dsl)
    }
  }

  protected def cast(m :Message) :Option[T] = { 
    m match { 
      case t :T => Some(t)
      case _    => None
    }
  }
  
  // action assumes that guard is true.
  final def action(m :T, senderLocation :ProtocolLocation, runtime :DSLRuntimeForEvents) = { 
    runtime.setSender(senderLocation)
    try { 
      performAction(m)
    } finally { 
      runtime.resetSender
    }
  }
  def performAction(msg :T)
}


abstract class CompositeEvent[T <: Message](implicit tag :ClassTag[T]) { 

  def performAction(t :Seq[T]) : Unit
  
  def isTriggered(set :Map[T,Seq[ProtocolLocation]], triggering :T) :Boolean
  
  def collect(messages :Map[T,Seq[ProtocolLocation]], triggering :T) : Map[T,Seq[ProtocolLocation]] 

  def checkAndExecute(messages :Map[Message,Seq[ProtocolLocation]], triggeringMessage :Message, runtime : DSLRuntimeForEvents) { 

    triggeringMessage match { 
      case triggering : T =>

	val typedMessages = messages.filterKeys { 
	  case key : T => true
	  case _ => false
	}.asInstanceOf[Map[T,Seq[ProtocolLocation]]]


	val tps = collect(typedMessages, triggering)
      

	if(isTriggered(tps, triggering)) { 

	  val ps = tps.values.map{ _.toSet } reduce { _ union _ }
	  val ts = tps.keys.toSeq
	  runtime.setSenders(ps)
	  
	  try { 
	    performAction(ts)
	  } finally { 
	    runtime.resetSenders 
	  }

	}
      case _ => ()
    }
  }
}
