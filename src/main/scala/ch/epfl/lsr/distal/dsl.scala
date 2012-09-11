package ch.epfl.lsr.distal

import ch.epfl.lsr.netty.protocol.ProtocolLocation
import reflect.ClassTag

trait DSL extends DSLforRuntime with DSLforEvents with DSLImplicits { 
  self  =>

  object UPON { 
    import language.experimental.macros
    def RECEIVING(caseObject :Any) :DSL.ReceivingBranch = macro DSLMacros.RECEIVING
  }

  def SEND[T <: Message](m :T) = { 
    new DSL.SENDbranch(m)
  }
}

object DSL { 
  trait ReceivingBranch { }
  trait DObranch extends UnTypedBranch { }
  trait CompositeMarker 

  class TypedReceivingBranch[T <: Message](val runtime :DSLRuntime)(implicit val tag :ClassTag[T]) extends DSL.ReceivingBranch with DSL.SimpleBranch[T] {
    val parent = null
  }

  trait UnTypedBranch { 
    def parent : UnTypedBranch
    
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

    override def getCompositeBranch :Option[CompositeBranch[T]] = { 
      if(parent==null)
	None
      else 
	parent.getCompositeBranch
    }

    def WITH(check :T=>Boolean) : WITHbranch[T,S] with S
    def FROM(from :Set[ProtocolLocation]) : FROMbranch[T,S] with S
    def FROM(from :ProtocolLocation*) : FROMbranch[T,S] with S = { 
      FROM(from.toSet)
    }


    def TIMES(times :Int) = { 
      new DSL.TIMESbranch[T](runtime, times, self)
    }

    //import language.experimental.macros
    //def DO(block :Any) : Unit = macro DSLMacros.DO 
  }

  trait CompositeBranch[T <: Message] extends TypedBranch[T,CompositeBranch[T]] with CompositeMarker { 
    def DO(block :Set[T]=>Unit) = new CompositeDOBranch[T](this).DO(block)

    override def getCompositeBranch = Some(this)

    def isTriggered(set :Set[(T,ProtocolLocation)]) : Boolean = { 
      parent match { 
	case cb : CompositeBranch[_] => cb.isTriggered(set)
	case _ => true
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
    def DO(block :T=>Unit) = new SimpleDOBranch[T](this).DO(block)

    def WITH(check :T=>Boolean) = { 
      new SimpleWITHbranch[T](runtime, check, this)
    }

    def FROM(from :Set[ProtocolLocation]) = { 
      new SimpleFROMbranch[T](runtime, from, this)
    }

  }

  // DSL "api" branches 
  class TIMESbranch[T <: Message](val runtime :DSLRuntime, times :Int, val parent :TypedBranch[T,_])(implicit val tag :ClassTag[T]) extends CompositeBranch[T] with CompositeMarker { 
    override def isTriggered(set :Set[(T,ProtocolLocation)]) : Boolean = { 
      super.isTriggered(set) && (set.size == times)
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
  class CompositeDOBranch[T <: Message](val parent : TypedBranch[T,_]) (implicit tag :ClassTag[T]) extends DObranch with CanGetCompositeBranch[T] { 
    def DO(block :Set[T]=>Unit) { 
      val cb = getCompositeBranch.get
      val e = new CompositeEvent[T] { 
	def performAction(t :Set[T]) = block(t)
	def filter(m :T, senderLocation :ProtocolLocation) = parent.guard(m, senderLocation)
	def isTriggered(set :Set[(T,ProtocolLocation)]) : Boolean = cb.isTriggered(set)
      }
      parent.runtime.addCompositeEvent(e)
    }
  }

  class SimpleDOBranch[T <: Message](val parent :TypedBranch[T,_]) (implicit tag :ClassTag[T]) extends DObranch { 
    def DO(block :T=>Unit) { 
      val e = new MessageEvent[T] { 
	def performAction(t :T) = block(t)
	override def guard(m :Message, senderLocation :ProtocolLocation) = { 
	  super.guard(m,senderLocation) && parent.guard(m, senderLocation)
	}
      }
      parent.runtime.addTriggeredEvent(e)
    }
  }

  // SEND
  class SENDbranch[T <: Message](message :T) { 
    def TO(seq: Seq[ProtocolLocation]) { 
      
    }
  }
}
