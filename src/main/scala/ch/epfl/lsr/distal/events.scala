package ch.epfl.lsr.distal

import ch.epfl.lsr.netty.protocol._
import reflect.{ClassTag}

trait DSLforEvents { 
  this :DSL =>

    final def senders : Set[ProtocolLocation] = __dslRuntime.senders.getOrElse { throw new Exception("senders is currently not defined") }
    final def sender :ProtocolLocation =  __dslRuntime.sender.getOrElse { throw new Exception("senders is currently not defined") }

}

trait DSLRuntimeForEvents { 
  var senders : Option[Set[ProtocolLocation]] = None
  var sender : Option[ProtocolLocation] = None
  
  def setSender(loc :ProtocolLocation) = { 
    if(sender.nonEmpty)
      throw new Exception("sender already set")
    else 
      sender = Some(loc)
  }
  def resetSender = { 
    sender = None
  }

  def setSenders(locs :Set[ProtocolLocation]) = { 
    if(senders.nonEmpty)
      throw new Exception("senders already set")
    else 
      senders = Some(locs)
  }
  def resetSenders = { 
    senders = None
  }

}


trait TriggeredEvent { 
  def guard(m :Message, senderLocation :ProtocolLocation) : Boolean
  def action(m :Message, senderLocation :ProtocolLocation, dsl: DSLRuntimeForEvents) : Unit
}

abstract class MessageEvent[T <: Message](implicit tag :ClassTag[T]) extends TriggeredEvent {  
  var sender :ProtocolLocation = null

  protected def cast(m :Message) :Option[T] = { 
    m match { 
      case t :T => Some(t)
      case _    => None
      
    }
  }
  
  // action assumes that guard is true.
  def action(m :Message, senderLocation :ProtocolLocation, runtime :DSLRuntimeForEvents) = { 
    this.synchronized { 
      runtime.setSender(senderLocation)
      //if(guard(m, senderLocation)) { 
      //println("guard OK: "+cast(m))
      //}
      cast(m).map{ performAction(_) }
      runtime.resetSender
    }
  }
  def performAction(msg :T)
  def guard(m :Message, senderLocation :ProtocolLocation)  = { 
    //println("MessageEvent.guard "+tag.runtimeClass.isInstance(m))
    m match { 
      case t :T => true
      case _ => false
    }
    //classTag[T].runtimeClass.isInstance(m) 
  }
}


abstract class CompositeEvent[T <: Message](implicit tag :ClassTag[T]) { 
  def performAction(t :Set[T]) : Unit
  def filter(t :T, p: ProtocolLocation) :Boolean
  def isTriggered(set :Set[(T,ProtocolLocation)]) :Boolean

  def action(pairs :Set[(Message,ProtocolLocation)]) = { 
  }

  def checkAndExecute(messages :Seq[(Message,ProtocolLocation)], runtime : DSLRuntimeForEvents) { 
    val tps = messages.collect { 
      case (m :T, p) if filter(m,p) => (m,p)
    }.toSet

    if(isTriggered(tps)) { 
      val (ts,ps) = tps.unzip
      this.synchronized { 
	runtime.setSenders(ps)
	performAction(ts)
	runtime.resetSenders 
      }
    }

  }
}
