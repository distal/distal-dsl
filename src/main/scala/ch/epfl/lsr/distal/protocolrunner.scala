package ch.epfl.lsr.distal

import java.net.InetSocketAddress

object DSLProtocolRunner { 
  def main(args :Array[String]) { 
    // first try ID on argument 
    if(args.size > 0) { 

      val id = args(0)
      createProtocol(id, ProtocolsConf.getClazz(id)).start

    } else { 

      // then try local Socketaddress as per dsl.local.address SystemProperty
      val prop = System.getProperty("dsl.local.address")
      if(prop!=null) { 
	val Array(host,port) = prop.split(":")
	val entries = ProtocolsConf.getEntriesForSocketAddress(new InetSocketAddress(host, port.toInt))
	entries.map { e => createProtocol(e.id, e.clazz, false) } map {
	  p => if(p!=null) p.start 
	}

      } else { 
	
	

	println("Not found any protocols to start. ")
	
      }
    }
  }

  private def getStringConstructor(clazz :Class[_]) :Option[java.lang.reflect.Constructor[_]] = { 
    try { 
      Some(clazz.getConstructor(classOf[String]))
    } catch { 
      case e :NoSuchMethodException =>None
    }
  }

  def createProtocol[_](ID :String, clazz :Option[Class[_]], failOnNone :Boolean = true) : DSLProtocol = { 
    clazz match { 
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
	}
      case None => 
	if(failOnNone) { 
	  throw new Exception("Cannot create protocol from ID "+ID+": no class set in config")
	} else { 
	  System.err.println("WARNING: Skipping creation of ID: not class set in config."); null
	}
    }
  }

}
