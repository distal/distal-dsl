package ch.epfl.lsr.distal

import ch.epfl.lsr.netty.config._
import ch.epfl.lsr.netty.protocol._

trait Message

object DSLProtocol { 
  def locationFromConfig(s :String) = new ProtocolLocation(ProtocolsConf.getURI(s))
}

abstract class DSLProtocol(val identifier :String) extends Protocol with DSL { 
  lazy val location :ProtocolLocation = DSLProtocol.locationFromConfig(identifier)
  
  def onMessageReceived(anyMsg :Any, remoteLocation :ProtocolLocation) = { 
    if(!anyMsg.isInstanceOf[Message])
      throw new Exception("received message is not of type Message: "+anyMsg+" ("+anyMsg.getClass+")")
    val msg = anyMsg.asInstanceOf[Message] 
    __dslRuntime.executeTriggeredEvents(msg, remoteLocation)
    __dslRuntime.storeMessage(msg, remoteLocation)
    __dslRuntime.executeCompositeEvents()
  }

}

