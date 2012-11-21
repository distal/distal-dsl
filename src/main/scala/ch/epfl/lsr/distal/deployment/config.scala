package ch.epfl.lsr.distal.deployment

import ch.epfl.lsr.distal._
import ch.epfl.lsr.netty.network.{ ProtocolLocation => NettyProtocolLocation }
import ch.epfl.lsr.protocol.ProtocolLocation
import java.net.{ URL, URI, InetSocketAddress }

// TODO: generalize this!

import scala.collection.Map

/*format of protocols.conf 
 * ProtocolId base-url
 * e.g.:
 * #comments are allowed
 * 1 ch.epfl.lsr.paxos.ClientStarter /Clients
 * -1 ch.epfl.lsr.paxos.Server /Server ch.epfl.lsr.paxos.Paxos /Paxos
 * # -1 means n-1
 */
object ProtocolsConf extends Resolver { 

  def getID(loc :ProtocolLocation) :String = locationMap(loc)
  def getAllLocations(clazz :Class[_]) :Seq[ProtocolLocation] = locationMap.keys.filter{  _.asInstanceOf[NettyProtocolLocation].isForClazz(clazz) }.toSeq

  def getLocations(ID :String) :Seq[NettyProtocolLocation] = configMap.getOrElse(ID, Seq())
  private def getLocation(ID :String, clazz :Class[_]) :Option[ProtocolLocation] = getLocations(ID).find { _.isForClazz(clazz) }


  lazy private val theMaps = readProtocolsConf
  lazy private val configMap :Map[String,Seq[NettyProtocolLocation]] = theMaps._1
  lazy private val locationMap :Map[ProtocolLocation,String] = theMaps._2
  lazy private val nodes :Array[String] = { 
    val nodesListURL = java.lang.System.getProperty("nodes.list.url")
    require(nodesListURL !=null, "nodes.list.url property not set")
    scala.io.Source.fromURL(nodesListURL).getLines.toArray
  }

  lazy private val protocolsConfURL :URL = { 
    val url = java.lang.System.getProperty("protocols.conf.url")
    require(url != null, "protocols.conf.url property not set") 
    new URL(url)
  }

  private case class ClassAndPath(clazz :String, path: String)

  private def str2loc(s :String)  :NettyProtocolLocation = new NettyProtocolLocation(s)

  private def parseLine(s :String) :Option[Tuple2[Int,Seq[ClassAndPath]]] = { 
    if ((s.length == 0) || (s startsWith "#"))
      return None

    val asList = s.split("[\t ]+").toList

    require(asList.length % 2 ==1, "parse error in protocols.conf file")
    
    var count = asList.head.toInt
    if(count <= 0)
      count = count + nodes.size

    val clazzpath = asList.tail.grouped(2).map { 
      l => 
	ClassAndPath(l.head, l.tail.head)
    }
    Some((count,clazzpath.toSeq))
  }

  private def doCommand(cps :Seq[ClassAndPath], startID :Int, nodes :List[String], acc :Map[String,Seq[NettyProtocolLocation]]) :Map[String,Seq[NettyProtocolLocation]] = 
    nodes match { 
      case Nil => acc
      case node::nodes => 
	val urls = cps.map { cp => str2loc("lsr://%s@%s%s".format(cp.clazz,node,cp.path)) }
	doCommand(cps, startID+1, nodes, acc updated (startID.toString,urls))
    }

  private def readProtocolsConf = { 
    val commands = scala.io.Source.fromURL(protocolsConfURL).getLines.flatMap(parseLine _)

    def loop(commands :List[Tuple2[Int,Seq[ClassAndPath]]], 
	     ID: Int = 1, 
	     remainingNodes :List[String] = nodes.toList, 
	     acc :Map[String,Seq[NettyProtocolLocation]] = Map.empty) :Map[String,Seq[NettyProtocolLocation]] = { 

      assume(remainingNodes.nonEmpty || commands.isEmpty, "not enough nodes to fullfill requirements of protocols.conf")
      
      commands match { 
	case Nil => acc
	case (n,cp)::tail => 
	  loop(tail,ID+n,remainingNodes drop n, doCommand(cp,ID,remainingNodes take n, acc))
      }
    }
    
    val theMap = loop(commands.toList)
    val locationMap = collection.mutable.HashMap.empty[ProtocolLocation,String] 
    
    theMap.foreach { 
      idlocs => 
	idlocs._2.foreach { 
	  { l => locationMap += ((l,idlocs._1)) }
	}
    }
    
    println(theMap)

    (theMap,locationMap)
  }
}
