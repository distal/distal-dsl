package ch.epfl.lsr.distal

import ch.epfl.lsr.netty.protocol.ProtocolLocation

import scala.collection.mutable.{ UnrolledBuffer, Buffer }

trait DSLforRuntime  { 
  this :DSL =>
  val __dslRuntime = new DSLRuntime(this)
}

class DSLRuntime(dsl :DSL) extends DSLRuntimeForEvents { 
  private val triggeredEvents = UnrolledBuffer[TriggeredEvent]()
  private val compositeEvents = UnrolledBuffer[CompositeEvent[_]]()
  private val messages = Buffer[Tuple2[Message,ProtocolLocation]]()

  def addTriggeredEvent(e :TriggeredEvent) { 
    triggeredEvents += e
  }

  def addCompositeEvent(e :CompositeEvent[_]) { 
    compositeEvents += e
  }
  
  def storeMessage(msg :Message, remoteLocation :ProtocolLocation) { 
    messages.append((msg, remoteLocation))
  }

  def executeCompositeEvents() { 
    compositeEvents.foreach { 
      _.checkAndExecute(messages, this)
    }
  }

  def executeTriggeredEvents(msg :Message, sender :ProtocolLocation) { 
    val events = triggeredEvents.filter { _.guard(msg, sender) }
    events foreach { _.action(msg, sender, this) }
  }
}
