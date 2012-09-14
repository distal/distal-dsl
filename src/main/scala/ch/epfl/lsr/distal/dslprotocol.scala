package ch.epfl.lsr.distal

import ch.epfl.lsr.netty.config._
import ch.epfl.lsr.netty.protocol._
import java.util.concurrent.TimeUnit._

trait Message

case class START() extends Message

object DSLProtocol { 
  def locationFromConfig(s :String) = ProtocolsConf.getLocation(s)
  def getAll(protocol :DSLProtocol) = ProtocolsConf.getAllLocationsWithClass(protocol.getClass)
}

trait DSLWithProtocol { 
  val __protocol : Protocol
}

abstract class DSLProtocol(val identifier :String) extends DSL with DSLWithProtocol { 
  self => 

    object __protocol extends Protocol { 
      lazy val location :ProtocolLocation = DSLProtocol.locationFromConfig(identifier)
      
      def onMessageReceived(anyMsg :Any, remoteLocation :ProtocolLocation) = { 
	if(!anyMsg.isInstanceOf[Message])
	  throw new Exception("received message is not of type Message: "+anyMsg+" ("+anyMsg.getClass+")")
	val msg = anyMsg.asInstanceOf[Message] 
	__dslRuntime.executeTriggeredEvents(msg, remoteLocation)
	__dslRuntime.storeMessage(msg, remoteLocation)
	__dslRuntime.executeCompositeEvents(msg)
      }

      override def afterStart = { 
	__protocol.onMessageReceived(new START(), __protocol.location)
      }
    }

  def start = __protocol.start
  def shutdown = __protocol.shutdown

  lazy val ALL :Array[ProtocolLocation] = DSLProtocol.getAll(self).toArray
}

