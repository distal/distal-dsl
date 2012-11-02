package ch.epfl.lsr.distal.deployment

import ch.epfl.lsr.distal._
import ch.epfl.lsr.netty.network.ProtocolLocation

import java.net.URI

object DSLProtocolRunner { 
  def main(args :Array[String]) { 

    try { 
      testURI
    } catch { 
      case t :Throwable =>
	println(t)
    }

    Resolver.setResolver(ProtocolsConf)

    // first try ID on argument 
    if(args.size > 0) { 

      val id = args(0)
      val protocols = createProtocols(id)
      println("Will start "+protocols.size+" protocols.")
      protocols.foreach{ 
	protocol => 
	  println(" "+protocol.getClass)
	  protocol.start
      }
      println("done starting")

    //}  else { 

      // DISABLED FOR NOW
      // then try local Socketaddress as per dsl.local.address SystemProperty
      // val prop = System.getProperty("dsl.local.address")
      // if(prop!=null) { 
      // 	val Array(host,port) = prop.split(":")
      // 	val entries = ProtocolsConf.getEntriesForSocketAddress(new InetSocketAddress(host, port.toInt))
      // 	entries.map { e => createProtocol(e.id, e.clazz, false) } map {
      // 	  p => if(p!=null) p.start 
      // 	}

      } else { 

	println("Not found any protocols to start. ")
	
      }
  }


  private def getStringConstructor(clazz :Class[_]) :Option[java.lang.reflect.Constructor[_]] = { 
    try { 
      Some(clazz.getConstructor(classOf[String]))
    } catch { 
      case e :NoSuchMethodException =>None
    }
  }

  def createProtocols(ID :String) : Seq[DSLProtocol] = { 
    val rv = ProtocolsConf.getLocations(ID).map { 
      loc => 
	createProtocol(ID, loc)
    } 
    rv
  }

  def createProtocol[_](ID :String, loc :ProtocolLocation, failOnNone :Boolean = true) : DSLProtocol = { 
    loc.clazz match { 
      case Some(clazz) => 
	try { 
	  getStringConstructor(clazz) match { 
	    case Some(cons) =>
	      cons.newInstance(ID).asInstanceOf[DSLProtocol]
	    case None =>
	      try { 
		clazz.newInstance.asInstanceOf[DSLProtocol]
	      } catch { 
		case e :NoSuchMethodException =>
		  throw new Exception("Cannot create protocol from ID "+ID+": class does not have suitable constructors")
	      }
	  }
	} finally { 
	  // println("creation finished")
	}

      case None => 
	if(failOnNone) { 
	  throw new Exception("Cannot create protocol from ID "+ID+": no class set in config")
	} else { 
	  System.err.println("WARNING: Skipping creation of ID: "+ID+" not class set in config: "+loc); null
	}
    }
  }

  def testURI { 
    val base = "lsr://ch.epfl.lsr.paxos.ClientStarter@chinqchint-45.lille.grid5000.fr/Client"
    val map = new scala.collection.mutable.HashMap[URI,Int]
    val size = 1000

    (1 to size) foreach { 
      i => 
	map.update(new URI(base+i), i) 
    }
    assume(map.size == size, "URI strangeness detected")


  }

}