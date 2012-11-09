package ch.epfl.lsr.distal

import ch.epfl.lsr.protocol.{ ProtocolLocation, Protocol }
import reflect.ClassTag
import ch.epfl.lsr.util.execution.{ Timer => TimerImpl }
import java.util.concurrent.TimeUnit

import scala.collection.{ Set, Map }


trait DSL extends DSLWithProtocol with DSLWithRuntime with DSLWithSenders with DSLImplicits with ImplicitDurations with DSLforMacros { 
  self =>

  object UPON { 
    import language.experimental.macros
    def RECEIVING(caseObject :Any) :DSL.ReceivingBranch = macro DSLMacros.RECEIVING
  }


  //object | { 
    def FORWARD[T <: Message](m :T) = { 
      new DSL.FORWARDbranch(m, self)
    }
  
    def SEND[T <: Message](m :T) = { 
      new DSL.SENDbranch(m, self)
    }
    
    def AFTER(duration :Duration) = { 
      new DSL.AFTERbranch(duration, self)
    }

    def DISCARD(msg :Message) = { 
      __runtime.removeMessage(msg)
    }

    def DISCARD(msgs :Seq[Message]) = { 
      // no branch. direct execution. 
      __runtime.removeMessages(msgs)
    }
    
    def TRIGGER[T <: Message](msg :T) = { 
      __protocol.fireMessageReceived(msg, __protocol.location)
    }

//    def TRIGGER[T <: Message](msg :{ def apply():T }) = { 
//      __protocol.fireMessageReceived(msg.apply(), __protocol.location)
//    }
  //}
  val | = self
}

object DSL { 
  trait ReceivingBranch { }
  trait DObranch[T <: Message] extends BasicTypedBranch[T] { }
  trait CompositeMarker 
  
  class TypedReceivingBranch[T <: Message](val protocol :DSLWithRuntime)(implicit val tag :ClassTag[T]) extends DSL.ReceivingBranch with DSL.SimpleBranch[T] {
    override val getSelf = null
    val parent = null
    def runtime :DSLRuntime = protocol.__runtime
  }

  trait BasicTypedBranch[T <: Message] { 
    def protocol :DSLWithRuntime
    val parent :BasicTypedBranch[T]
    implicit val tag :ClassTag[T] 

    def guard(m: T, p :ProtocolLocation) : Boolean = true
    
    final def checkGuardsRecursively(m :T, senderLocation :ProtocolLocation) = { 
      var p : BasicTypedBranch[T] = parent
      var rv = true
      while((p!=null) && rv) { 
	rv = p.guard(m, senderLocation)
	p = p.parent
      }
      rv
    }
  }

  trait TypedBranch[T <: Message,S] extends BasicTypedBranch[T] { 
    self =>

    def WITH(check :T=>Boolean) : WITHbranch[T,S] with S
    def FROM(from :Set[ProtocolLocation]) : FROMbranch[T,S] with S

    def FROM(from :ProtocolLocation*) : FROMbranch[T,S] with S = FROM(from.toSet)
    // def FROM(from :String*) :FROMbranch[T,S] with S = FROM(from.toArray)
    // def FROM(from :Set[String]) : FROMbranch[T,S] with S = 
    //   FROM(from.toSet.map { s :String => DSLProtocol.locationForId(protocol, s).get })
    

    def TIMES(times : =>Int) = { 
      new DSL.TIMESbranch[T](protocol, times, self)
    }

    def SAME[R](extractor :T=>R) = { 
      new DSL.SAMEbranch[T,R](protocol, extractor, self)
    }

    //import language.experimental.macros
    //def DO(block :Any) : Unit = macro DSLMacros.DO 
  }

  trait CompositeBranch[T <: Message] extends TypedBranch[T,CompositeBranch[T]] with CompositeMarker { 
    def DO(block :Seq[T]=>Unit) = new CompositeDOBranch[T](this, protocol).DO(block)

    def isTriggered(set :Map[T,Seq[ProtocolLocation]], triggering :T) : Boolean = true  

    def collect(messages :Map[T,Seq[ProtocolLocation]], triggering :T) :Map[T,Seq[ProtocolLocation]] = messages
    def composeSets(sets :Seq[Seq[(T,ProtocolLocation)]]) : Seq[Seq[(T,ProtocolLocation)]] = sets 

    def WITH(check :T=>Boolean) = { 
      new CompositeWITHbranch[T](protocol, check, this) 
    }

    def FROM(from :Set[ProtocolLocation]) = { 
      new CompositeFROMbranch[T](protocol, from, this)
    }

  }

  trait SimpleBranch[T <: Message] extends TypedBranch[T,SimpleBranch[T]] { 
    def getSelf = this

    def DO(block :T=>Unit) = new SimpleDOBranch[T](getSelf, protocol).DO(block)

    def WITH(check :T=>Boolean) = { 
      new SimpleWITHbranch[T](protocol, check, getSelf)
    }

    def FROM(from :Set[ProtocolLocation]) = { 
      new SimpleFROMbranch[T](protocol, from, getSelf)
    }

  }

  // DSL "api" branches 
  class TIMESbranch[T <: Message](val protocol :DSLWithRuntime, times : =>Int, val parent :TypedBranch[T,_])(implicit val tag :ClassTag[T]) extends CompositeBranch[T] with CompositeMarker { 
    override def isTriggered(set :Map[T,Seq[ProtocolLocation]], triggering: T) : Boolean = { 
      val count = set.values.reduceOption(_ ++ _).map{ _.size}.getOrElse(0)
      //val count :Int = set.foldLeft(0)( (i,entry) => i+entry._2.size )
      (count == times)
    } 

    override def composeSets(sets :Seq[Seq[(T,ProtocolLocation)]]) : Seq[Seq[(T,ProtocolLocation)]] = { 
      sets.flatMap { 
	_.combinations(times).toSeq
      }
    }
  }  

  class SAMEbranch[T <: Message, R](val protocol :DSLWithRuntime, extractor :T=>R, val parent :TypedBranch[T,_])(implicit val tag :ClassTag[T]) extends CompositeBranch[T] with CompositeMarker {  
    override def collect(messages :Map[T,Seq[ProtocolLocation]], triggering :T) = { 
      val triggeringValue :R = extractor(triggering)

      messages.filter(kv => extractor(kv._1)==triggeringValue)
      
      // for { 
      // 	(msg, senders) <- messages
      // 	if(extractor(msg) == triggeringValue)
      // } yield (msg, senders)
    }

    override def composeSets(sets :Seq[Seq[(T,ProtocolLocation)]]) : Seq[Seq[(T,ProtocolLocation)]] = { 
      sets.flatMap { 
	set => 
	  val typedPairs :Seq[(T,ProtocolLocation)] = set.flatMap { 
	    pair => pair._1 match { 
	      case m:T => Some((m,pair._2))
	      case _ => None
	    } 
	  }
	typedPairs.groupBy(pair => extractor(pair._1)).values.toSeq
      }
    }
  }


  trait WITHbranch[T <: Message,S] extends TypedBranch[T,S] { 
    val check :(T=>Boolean)
    override def guard(m :T, senderLocation :ProtocolLocation) = { 
      check(m)
    }
  }


  trait FROMbranch[T <: Message,S] extends TypedBranch[T,S] { 
    val sources :Set[ProtocolLocation]
    override def guard(m :T, senderLocation :ProtocolLocation) = { 
      (sources contains senderLocation)
    }
  }


  // [Simple|Composite][WITH|FROM]branch
  class SimpleWITHbranch[T <: Message](val protocol :DSLWithRuntime, val check :(T=>Boolean), val parent :TypedBranch[T,_])(implicit val tag :ClassTag[T]) extends WITHbranch[T,SimpleBranch[T]] with SimpleBranch[T]

  class CompositeWITHbranch[T <: Message](val protocol :DSLWithRuntime, val check :(T=>Boolean), val parent :TypedBranch[T,_])(implicit val tag :ClassTag[T]) extends WITHbranch[T,CompositeBranch[T]] with CompositeBranch[T]
  

  class SimpleFROMbranch[T <: Message](val protocol :DSLWithRuntime, val sources :Set[ProtocolLocation], val parent :TypedBranch[T,_]) (implicit val tag :ClassTag[T]) extends FROMbranch[T,SimpleBranch[T]] with SimpleBranch[T]

  class CompositeFROMbranch[T <: Message](val protocol :DSLWithRuntime, val sources :Set[ProtocolLocation], val parent :TypedBranch[T,_]) (implicit val tag :ClassTag[T]) extends FROMbranch[T,CompositeBranch[T]] with CompositeBranch[T]



  private def cast[T <: Message](m :Message)(implicit tag:ClassTag[T]) :Option[T] = { 
    m match { 
      case t :T => Some(t)
      case _    => None
    }
  }

  
 // DO
  class CompositeDOBranch[T <: Message](val parent : TypedBranch[T,_], val protocol :DSLWithRuntime) (implicit val tag :ClassTag[T]) extends DObranch[T] { 
    self=>

    import scala.collection.mutable.ArrayBuffer

    lazy val parentsSeqs = { 
      var cb :ArrayBuffer[CompositeBranch[T]] = ArrayBuffer.empty
      var all : ArrayBuffer[BasicTypedBranch[T]] = ArrayBuffer.empty
      var p :BasicTypedBranch[T] = parent
      while(p!=null) {
	all += p
	p match { 
	  case cbp : CompositeBranch[T] => 
	    cb += cbp
	  case _ => ()
	}
	p = p.parent
      }
      (all,cb)
    }
    
    lazy val allParents = parentsSeqs._1
    lazy val compositeParents = parentsSeqs._2
    
    def DO(block :Seq[T]=>Unit) { 

      val e = new CompositeEvent[T] { 
	import scala.collection.{ Set, Map }
	
	def performAction(t :Seq[T]) = block(t)

	private def matchGuards(messages :Map[T,Seq[ProtocolLocation]]) = { 
	  for { 
	    (msg, allSenders) <- messages
	    selectedSenders = allSenders.filter { sender => self.checkGuardsRecursively(msg,sender) }
	    if selectedSenders.nonEmpty
	  } yield(msg, selectedSenders)
	}  

	def collect(messages :Map[T,Seq[ProtocolLocation]], triggering :T) = { 
	  val guardsMatched = matchGuards(messages)	    
	  //  println("%d have matched the guards".format(guardsMatched.size))
	  val collected = compositeParents.foldLeft(guardsMatched) { (msgs,cb) => cb.collect(msgs, triggering) }
	  // println("%d have been collected".format(collected.size))
	  collected
	}
	
	def isTriggered(set :Map[T,Seq[ProtocolLocation]], triggering :T) : Boolean = { 
	  compositeParents.forall(_.isTriggered(set, triggering))
	}

	def composeSets(sets :Seq[Seq[(T,ProtocolLocation)]]) : Seq[Seq[(T,ProtocolLocation)]] = 
	  compositeParents.foldLeft(sets) { (sets,parent) => parent.composeSets(sets) }

      }
      protocol.__runtime.addCompositeEvent(e)
    }
  }

  class SimpleDOBranch[T <: Message](val parent :TypedBranch[T,_], val protocol :DSLWithRuntime) (implicit val tag :ClassTag[T]) extends DObranch[T] { 
    self=>

    def DO(block :T=>Unit) { 
      val e = new MessageEvent[T] { 
	def performAction(t :T) = block(t)
	override def guard(m :T, senderLocation :ProtocolLocation) = self.checkGuardsRecursively(m, senderLocation)
      }
      protocol.__runtime.addTriggeredEvent(e)
    }
  }

  // SEND
  class SENDbranch[T <: Message](message :T, protocol :DSLWithProtocol) { 
    def TO(seq: ProtocolLocation*) { 
      protocol.__protocol.sendTo(message,seq :_*)
    }
    def TO(seq: Array[ProtocolLocation]) { 
      TO(seq.toSeq :_*)
    }

    def TO(dst :String) { 
      val dstId :ProtocolLocation = DSLProtocol.locationForId(protocol, dst).get
      protocol.__protocol.sendTo(message, dstId)
    }

    def TO(dst :DSLProtocol) { 
      // protocol.__protocol.network.sendTo(message, dst.__protocol.location)
      dst.__protocol.fireMessageReceived(message, protocol.__protocol.location)
    }
  }

  // FORWARD
  class FORWARDbranch[T <: Message](message :T, protocol :DSL) { 
    def TO(location: ProtocolLocation) { 
      protocol.__protocol.forwardTo(message, location, protocol.SENDER)
    }

    def TO(dst :String) { 
      val dstId :ProtocolLocation = DSLProtocol.locationForId(protocol, dst).get
      protocol.__protocol.forwardTo(message, dstId, protocol.SENDER)
    }

    def TO(dst :DSLProtocol) { 
      // protocol.__protocol.network.sendTo(message, dst.__protocol.location)
      dst.__protocol.fireMessageReceived(message, protocol.SENDER)
    }
  }
  


  // AFTER
  class AFTERbranch(duration :Duration, dsl :DSL) { 
    def DO(thunk : => Unit) = { 
      TimerImpl.delay(duration.amount, duration.unit) { 
	if(!dsl.__protocol.isShutdown)
	  dsl.__protocol.inPool(thunk)
      }
    }
  }

  

}
