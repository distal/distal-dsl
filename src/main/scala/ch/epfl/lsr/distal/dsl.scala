package ch.epfl.lsr.distal

import ch.epfl.lsr.netty.protocol.{ ProtocolLocation, Protocol }
import reflect.ClassTag
import ch.epfl.lsr.netty.util.{ Timer => TimerImpl }
import java.util.concurrent.TimeUnit

import scala.collection.{ Set, Map }


trait DSL extends DSLforRuntime with DSLforEvents with DSLImplicits with DSLWithProtocol with ImplicitDurations { 
  self =>

  object UPON { 
    import language.experimental.macros
    def RECEIVING(caseObject :Any) :DSL.ReceivingBranch = macro DSLMacros.RECEIVING
  }

  object | { 
    def SEND[T <: Message](m :T) = { 
      new DSL.SENDbranch(m, __protocol)
    }
    
    def AFTER(duration :Duration) = { 
      new DSL.AFTERbranch(duration, self)
    }

    def DISCARD(msg :Message) = { 
      __dslRuntime.removeMessage(msg)
    }

    def DISCARD(msgs :Seq[Message]) = { 
      // no branch. direct execution. 
      __dslRuntime.removeMessages(msgs)
    }
  }
}

object DSL { 
  trait ReceivingBranch { }
  trait DObranch extends UnTypedBranch { }
  trait CompositeMarker 
  
  class TypedReceivingBranch[T <: Message](val runtime :DSLRuntime)(implicit val tag :ClassTag[T]) extends DSL.ReceivingBranch with DSL.SimpleBranch[T] {
    override val getSelf = null
    val parent = null
  }

  trait UnTypedBranch { 
    def parent : UnTypedBranch
    def runtime : DSLRuntime

    def guard(m: Message, p :ProtocolLocation) : Boolean = { 
      if(parent!=null)
	parent.guard(m, p)
      else
	true
    }
  }

  trait CanGetCompositeBranch[T <: Message] extends UnTypedBranch{ 
    def parent : CanGetCompositeBranch[T]

    def getCompositeBranch :Option[CompositeBranch[T]] = { 
      if(parent==null)
	None
      else 
	parent.getCompositeBranch
    }
  }

  trait TypedBranch[T <: Message,S] extends CanGetCompositeBranch[T] { 
    self =>
    def runtime : DSLRuntime
    val parent :TypedBranch[T,_]
    implicit val tag :ClassTag[T] 


    def WITH(check :T=>Boolean) : WITHbranch[T,S] with S
    def FROM(from :Set[ProtocolLocation]) : FROMbranch[T,S] with S
    def FROM(from :ProtocolLocation*) : FROMbranch[T,S] with S = { 
      FROM(from.toSet)
    }


    def TIMES(times :Int) = { 
      new DSL.TIMESbranch[T](runtime, times, self)
    }

    def SAME[R](extractor :T=>R) = { 
      new DSL.SAMEbranch[T,R](runtime, extractor, self)
    }

    //import language.experimental.macros
    //def DO(block :Any) : Unit = macro DSLMacros.DO 
  }

  trait CompositeBranch[T <: Message] extends TypedBranch[T,CompositeBranch[T]] with CompositeMarker { 
    def DO(block :Seq[T]=>Unit) = new CompositeDOBranch[T](this, runtime).DO(block)

    override def getCompositeBranch = Some(this)

    val parentIsComposite = parent match { 
      case cb:CompositeBranch[T] => true
      case _ => false
    }

    def isTriggered(set :Map[T,Seq[ProtocolLocation]], triggering :T) : Boolean = { 
      if(parentIsComposite) 
	parent.asInstanceOf[CompositeBranch[T]].isTriggered(set, triggering)
      else 
	true
    }

    def collect(messages :Map[T,Seq[ProtocolLocation]], triggering :T) :Map[T,Seq[ProtocolLocation]] = { 
      if(parentIsComposite) { 
	  parent.asInstanceOf[CompositeBranch[T]].collect(messages, triggering)
      } else { 
	for { 
	  (msg, allSenders) <- messages
	  selectedSenders = allSenders.filter { sender => parent.guard(msg,sender) }
	  if selectedSenders.nonEmpty
	} yield(msg, selectedSenders)
      }
    }

    def WITH(check :T=>Boolean) = { 
      new CompositeWITHbranch[T](runtime, check, this) 
    }

    def FROM(from :Set[ProtocolLocation]) = { 
      new CompositeFROMbranch[T](runtime, from, this)
    }

  }

  trait SimpleBranch[T <: Message] extends TypedBranch[T,SimpleBranch[T]] { 
    def getSelf = this

    def DO(block :T=>Unit) = new SimpleDOBranch[T](getSelf, runtime).DO(block)

    def WITH(check :T=>Boolean) = { 
      new SimpleWITHbranch[T](runtime, check, getSelf)
    }

    def FROM(from :Set[ProtocolLocation]) = { 
      new SimpleFROMbranch[T](runtime, from, getSelf)
    }

  }

  // DSL "api" branches 
  class TIMESbranch[T <: Message](val runtime :DSLRuntime, times :Int, val parent :TypedBranch[T,_])(implicit val tag :ClassTag[T]) extends CompositeBranch[T] with CompositeMarker { 
    override def isTriggered(set :Map[T,Seq[ProtocolLocation]], triggering: T) : Boolean = { 
      val count :Int = set.map( _._2.size ).sum

      super.isTriggered(set, triggering) && (count == times)
    } 
  }  

  class SAMEbranch[T <: Message, R](val runtime :DSLRuntime, extractor :T=>R, val parent :TypedBranch[T,_])(implicit val tag :ClassTag[T]) extends CompositeBranch[T] with CompositeMarker {  
    override def collect(messages :Map[T,Seq[ProtocolLocation]], triggering :T) = { 
      val triggeringValue :R = extractor(triggering)
      for { 
	(msg, senders) <- super.collect(messages, triggering)
	extracted = extractor(msg)
	if(extracted == triggeringValue)
      } yield (msg, senders)
    }
  }


  trait WITHbranch[T <: Message,S] extends TypedBranch[T,S] { 
    val check :(T=>Boolean)
    override def guard(m :Message, senderLocation :ProtocolLocation) = { 
      super.guard(m,senderLocation) && (cast(m).filter(check).nonEmpty)
    }
  }


  trait FROMbranch[T <: Message,S] extends TypedBranch[T,S] { 
    val sources :Set[ProtocolLocation]
    override def guard(m :Message, senderLocation :ProtocolLocation) = { 
      super.guard(m,senderLocation) && (sources contains senderLocation)
    }
  }


  // [Simple|Composite][WITH|FROM]branch
  class SimpleWITHbranch[T <: Message](val runtime :DSLRuntime, val check :(T=>Boolean), val parent :TypedBranch[T,_])(implicit val tag :ClassTag[T]) extends WITHbranch[T,SimpleBranch[T]] with SimpleBranch[T]

  class CompositeWITHbranch[T <: Message](val runtime :DSLRuntime, val check :(T=>Boolean), val parent :TypedBranch[T,_])(implicit val tag :ClassTag[T]) extends WITHbranch[T,CompositeBranch[T]] with CompositeBranch[T]
  

  class SimpleFROMbranch[T <: Message](val runtime :DSLRuntime, val sources :Set[ProtocolLocation], val parent :TypedBranch[T,_]) (implicit val tag :ClassTag[T]) extends FROMbranch[T,SimpleBranch[T]] with SimpleBranch[T]

  class CompositeFROMbranch[T <: Message](val runtime :DSLRuntime, val sources :Set[ProtocolLocation], val parent :TypedBranch[T,_]) (implicit val tag :ClassTag[T]) extends FROMbranch[T,CompositeBranch[T]] with CompositeBranch[T]



  private def cast[T <: Message](m :Message)(implicit tag:ClassTag[T]) :Option[T] = { 
    m match { 
      case t :T => Some(t)
      case _    => None
    }
  }

  
 // DO
  class CompositeDOBranch[T <: Message](val parent : TypedBranch[T,_], val runtime :DSLRuntime) (implicit tag :ClassTag[T]) extends DObranch with CanGetCompositeBranch[T] { 
    def DO(block :Seq[T]=>Unit) { 
      val cb = getCompositeBranch.get
      val e = new CompositeEvent[T] { 
	import scala.collection.{ Set, Map }

	def performAction(t :Seq[T]) = block(t)
	def collect(messages :Map[T,Seq[ProtocolLocation]], triggering :T) = cb.collect(messages, triggering)
	def isTriggered(set :Map[T,Seq[ProtocolLocation]], triggering :T) : Boolean = cb.isTriggered(set, triggering)
      }
      runtime.addCompositeEvent(e)
    }
  }

  class SimpleDOBranch[T <: Message](val parent :TypedBranch[T,_], val runtime :DSLRuntime) (implicit tag :ClassTag[T]) extends DObranch { 
    self=>

    def DO(block :T=>Unit) { 
      val e = new MessageEvent[T] { 
	def performAction(t :T) = block(t)
	override def guard(m :T, senderLocation :ProtocolLocation) = { 
	  self.guard(m,senderLocation)
	}
      }
      runtime.addTriggeredEvent(e)
    }
  }

  // SEND
  class SENDbranch[T <: Message](message :T, protocol :Protocol) { 
    def TO(seq: ProtocolLocation*) { 
      protocol.network.sendTo(message,seq :_*)
    }
    def TO(seq: Array[ProtocolLocation]) { 
      protocol.network.sendTo(message,seq :_*)
    }

    def TO(dst :DSLProtocol) { 
      protocol.network.sendTo(message, dst.__protocol.location)
      // cannot short-cut so much (because we are likely inside a event)
      //dst.__protocol.onMessageReceived(message, protocol.location)
    }
  }

  // AFTER

  class AFTERbranch(duration :Duration, dsl :DSL) { 
    def DO(thunk : => Unit) = { 
      // ALERT: rather than execute, this should put into protocol Q!
      TimerImpl.delay(duration.amount, duration.unit) { 
	dsl.__protocol.inPool(thunk)
      }
    }
  }

  

}
