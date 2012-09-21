package ch.epfl.lsr.distal

import ch.epfl.lsr.netty.protocol.ProtocolLocation

import scala.collection.mutable.{ UnrolledBuffer, ArrayBuffer, MultiMap, HashMap, Set, Map }

trait DSLforRuntime  { 
  this :DSL =>
  val __dslRuntime = new DSLRuntime(this)
}

class DSLRuntime(dsl :DSL) extends DSLRuntimeForEvents { 
  private val triggeredEvents = UnrolledBuffer[MessageEvent[_]]()
  private val compositeEvents = UnrolledBuffer[CompositeEvent[_]]()
  private val messages = new HashMap[Message, ArrayBuffer[ProtocolLocation]]
  
  def addTriggeredEvent(e :MessageEvent[_]) { 
    triggeredEvents += e
  }

  def addCompositeEvent(e :CompositeEvent[_]) { 
    compositeEvents += e
  }
  
  def storeMessage(msg :Message, remoteLocation :ProtocolLocation) { 
    messages.get(msg) match { 
      case None => 
	val ab = new ArrayBuffer[ProtocolLocation]
	ab += remoteLocation
	messages(msg) = ab
      case Some(seq) =>
	seq += remoteLocation
    }
  }

  def removeMessages(msgs :Seq[Message]) { 
    messages --= msgs
  }

  def removeMessage(msg :Message) { 
    messages -= msg
  }

  def executeCompositeEvents(lastMessage :Message) { 
    compositeEvents.foreach { 
      _.checkAndExecute(messages, lastMessage, this)
    }
  }

  def executeTriggeredEvents(msg :Message, sender :ProtocolLocation) { 
    triggeredEvents.foreach { 
      _.checkAndExecute(msg, sender, this)
    }
  }
}
