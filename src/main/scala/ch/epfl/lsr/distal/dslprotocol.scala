package ch.epfl.lsr.distal

import ch.epfl.lsr.netty.config._
import ch.epfl.lsr.netty.protocol._
import java.util.concurrent.TimeUnit._

/*
 * TODO: merge __protocol and __runtime into ???
 *
 */

trait Message

case class START() extends Message
case class SHUTDOWN() extends Message
case class STATECHANGED() extends Message

object DSLProtocol { 
  def locationForId(protocol :AnyRef, id :String) :Option[ProtocolLocation] = 
    ProtocolsConf.getLocation(id, protocol.getClass)
  def locationForId[T <:DSLProtocol](clazz :Class[T], id :String) :ProtocolLocation =     
    ProtocolsConf.getLocation(id, clazz).get
  def idForLocation(loc :ProtocolLocation) :String =         
    ProtocolsConf.getID(loc)
  def getAll(protocol :DSLProtocol) :Seq[ProtocolLocation] = 
    getAll(protocol.getClass)
  def getAll(clazz :Class[_]) :Seq[ProtocolLocation] = 
    ProtocolsConf.getAllLocations(clazz)

}

trait DSLWithProtocol { 
  val __protocol :Protocol
}

trait DSLProtocol extends DSL { 
  self => 
    val ID :String

    val __runtime = __protocol
    object __protocol extends Protocol with DSLRuntime { 
      val DSL = self
      // this goes through enclosing DSLProtocol so, that it can be overriden
      lazy val location :ProtocolLocation = LOCATION 
      
      def onMessageReceived(anyMsg :Any, remoteLocation :ProtocolLocation) = { 
	if(!anyMsg.isInstanceOf[Message])
	  throw new Exception("received message is not of type Message: "+anyMsg+" ("+anyMsg.getClass+")")
	val msg = anyMsg.asInstanceOf[Message] 
	
	storeMessage(msg, remoteLocation)
	executeTriggeredEvents(msg, remoteLocation)
	executeCompositeEvents(msg)
      }

      override def afterStart = { 
	fireMessageReceived(new START(), location)
      }

      override def beforeShutdown :Unit = { 
	fireMessageReceived(new SHUTDOWN(), location)
      }

      lazy val ALL = DSLProtocol.getAll(self).toArray
      lazy val ALL_REMOTE = DSLProtocol.getAll(self).filterNot(_ == LOCATION).toArray
      lazy val __DEFAULT_LOCATION_from_config :Option[ProtocolLocation] = DSLProtocol.locationForId(self, ID)
    }

  // "external" API
  final def start = __protocol.start
  final def shutdown = __protocol.shutdown

  // just forward these into __protocol, which provides the default (caching) implementations.
  // avoided here, so they can be overriden.
  def ALL :Array[ProtocolLocation] = __protocol.ALL
  def ALL_REMOTE :Array[ProtocolLocation] = __protocol.ALL_REMOTE
  def LOCATION :ProtocolLocation = __protocol.__DEFAULT_LOCATION_from_config.get


  // UPON STATECHANGED 
  new DSL.TypedReceivingBranch[STATECHANGED](self).DO { 
    msg => 
      DISCARD(msg)
      val sender = SENDER
      __runtime.resetSender
      __runtime.reapplyStoredMessages
      __runtime.setSender(sender)
  }
}

