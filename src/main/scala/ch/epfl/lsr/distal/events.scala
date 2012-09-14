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
      try { 
      //if(guard(m, senderLocation)) { 
      //println("guard OK: "+cast(m))
      //}
	cast(m).map{ performAction(_) }
      } finally { 
	runtime.resetSender
      }
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
  def performAction(t :Seq[T]) : Unit
//  def filter(t :T, p: ProtocolLocation) :Boolean
  def isTriggered(set :Seq[(T,ProtocolLocation)], triggering :T) :Boolean

  def collect(messages :Seq[(Message,ProtocolLocation)], triggering :T) :Seq[(T,ProtocolLocation)] 

  def checkAndExecute(messages :Seq[(Message,ProtocolLocation)], triggeringMessage :Message, runtime : DSLRuntimeForEvents) { 
    triggeringMessage match { 
      case triggering : T =>
	val tps = collect(messages, triggering).toSeq
	if(isTriggered(tps, triggering)) { 
	  val (ts,ps) = tps.unzip
	  this.synchronized { 
	    runtime.setSenders(ps.toSet)
	    try { 
	      performAction(ts)
	    } finally { 
	      runtime.resetSenders 
	    }
	  }
	}
      case _ => ()
    }
  }
}
