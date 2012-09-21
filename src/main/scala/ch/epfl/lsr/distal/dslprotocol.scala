package ch.epfl.lsr.distal

import ch.epfl.lsr.netty.config._
import ch.epfl.lsr.netty.protocol._
import java.util.concurrent.TimeUnit._

trait Message

case class START() extends Message

object DSLProtocol { 
  def locationForId(s :String) = ProtocolsConf.getLocation(s)
  def idForLocation(loc :ProtocolLocation) :String = ProtocolsConf.getIdForLocation(loc)
  def getAll(protocol :DSLProtocol) = ProtocolsConf.getAllLocationsWithClass(protocol.getClass)
}

trait DSLWithProtocol { 
  val __protocol : Protocol
}

abstract class DSLProtocol(val identifier :String) extends DSL with DSLWithProtocol { 
  self => 

    object __protocol extends Protocol { 
      lazy val location :ProtocolLocation = DSLProtocol.locationForId(identifier)
      //var prev = 0L
      
      def onMessageReceived(anyMsg :Any, remoteLocation :ProtocolLocation) = { 
	//val start = System.nanoTime
	//println("no message received for "+((start-prev)/1000)+" micros")


	if(!anyMsg.isInstanceOf[Message])
	  throw new Exception("received message is not of type Message: "+anyMsg+" ("+anyMsg.getClass+")")
	val msg = anyMsg.asInstanceOf[Message] 
	__dslRuntime.storeMessage(msg, remoteLocation)
	
	__dslRuntime.executeTriggeredEvents(msg, remoteLocation)
	
	//val afterTriggers = System.nanoTime
	
	
	//val beforeComposite = System.nanoTime
	__dslRuntime.executeCompositeEvents(msg)

	//prev = System.nanoTime
	//println("receiving "+anyMsg+" from "+remoteLocation+" took "+((prev-start)/1000)+" micros\n("+((afterTriggers-start)/1000)+" micros for triggered and "+((prev-beforeComposite)/1000)+"micros for composite)")
      }

      override def afterStart = { 
	__protocol.onMessageReceived(new START(), __protocol.location)
      }
    }

  def start = __protocol.start
  def shutdown = __protocol.shutdown

  lazy val ALL :Array[ProtocolLocation] = DSLProtocol.getAll(self).toArray
}

