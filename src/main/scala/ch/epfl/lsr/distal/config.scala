package ch.epfl.lsr.distal

import ch.epfl.lsr.netty.config._
import java.net.URL 

/*format of protocols.conf 
 * ProtocolId url
 * e.g.:
 * ProtocolInstance1 lsr://localhost:8080/Protocol
 * ProtocolInstance2 lsr://localhost:8008/Protocol
 *
 * TODO: fallback to normal config protcols.ProtocolInstance1
 */
object ProtocolsConf { 
  lazy private val configMap :Map[String,String] = readMap
  lazy private val configURL :URL = { 
    Configuration.getMap("dsl").get("procotolsconf") match { 
      case Some(u :String) => new URL(u)
      case _ => 
	val u = getClass.getClassLoader().getResource("protocols.conf")
	if(u==null) { 
	  throw new Exception("dsl.protolsconf not set and default protocols.conf not found")
	}
	u
    }
  }
  
  def get(protocolName :String) = { configMap(protocolName)  }
  def getURI(protocolName :String) = new java.net.URI(get(protocolName))

  import scala.util.matching.Regex
  val ConfigEntry = """(.*) (.*)""".r

  private def splitToPair(s :String) = { 
    val ConfigEntry(name,location) = s
    (name,location)
  }

  private def readMap :Map[String,String] = { 
    val lines = scala.io.Source.fromURL(configURL).getLines

    val themap = lines.map(splitToPair _).toMap

    themap
  }
}
