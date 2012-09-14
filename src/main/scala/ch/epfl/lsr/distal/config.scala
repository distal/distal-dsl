package ch.epfl.lsr.distal

import ch.epfl.lsr.netty.config._
import ch.epfl.lsr.netty.protocol.ProtocolLocation
import java.net.{ URL, URI, InetSocketAddress }

case class ProtocolsConfigEntry(id: String, location :ProtocolLocation, clazz :Option[Class[_]])

/*format of protocols.conf 
 * ProtocolId url [ProtocolClass]
 * e.g.:
 * ProtocolInstance1 lsr://localhost:8080/Protocol 
 * ProtocolInstance2 lsr://localhost:8008/Protocol 
 *
 * TODO: fallback to normal config protcols.ProtocolInstance1
 */
object ProtocolsConf { 
  lazy private val configMap :Map[String,ProtocolsConfigEntry] = readMap
  lazy private val configURL :URL = { 
    Configuration.getMap("dsl").get("procotolsconf") match { 
      case Some(u :String) => new URL(u)
      case _ => 
	var filename = "protocols.conf"
	if(java.lang.System.getProperty("protocols.conf.file")!=null) { 
	  filename = java.lang.System.getProperty("protocols.conf.file")
	}
	val u = this.getClass.getClassLoader.getResource(filename)
	if(u==null) { 
	  throw new Exception("dsl.protolsconf not set and default protocols.conf not found")
	}
	u
    }
  }

  
  def get(protocolId :String) = { configMap(protocolId)  }
  def getLocation(protocolId :String) = get(protocolId).location
  def getClazz(protocolId :String) = get(protocolId).clazz

  def getAllEntries = configMap.values

  def getEntriesForSocketAddress(saddr :InetSocketAddress) = { 
    val port = saddr.getPort
    getEntriesForHostname(saddr.getHostName).filter { _.location.port == port }
  }
  
  def getEntriesForHostname(hostname :String) = { 
    configMap.values.filter { _.location.host == hostname }
  }

  private def str2loc(s :String)  :ProtocolLocation = { 
    new ProtocolLocation(new URI(s))
  }

  def getAllLocationsWithClass(clazz :Class[_]) = { 
    configMap.values.filter { _.clazz match { 
      case Some(c) if c == clazz => true 
      case _ => false
    }} map { _.location }
  }
  def getAllLocations = configMap.values map { _.location }

  import scala.util.matching.Regex
  val ConfigEntryLine = """(.*?)\s(.*?)(?:\s(.*))?""".r

  private def splitToPair(s :String) = { 
    val ConfigEntryLine(id,location,className) = s
    val clazz :Option[Class[_]] = if(className ==null) None else Some(Class.forName(className))
    (id,new ProtocolsConfigEntry(id, str2loc(location),clazz))
  }

  private def readMap :Map[String,ProtocolsConfigEntry] = { 
    val lines = scala.io.Source.fromURL(configURL).getLines

    val themap = lines.filterNot( _ startsWith "#" ).map(splitToPair _).toMap

    themap
  }
}
