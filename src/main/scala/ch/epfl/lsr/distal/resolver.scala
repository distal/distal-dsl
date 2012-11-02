package ch.epfl.lsr.distal

import ch.epfl.lsr.protocol.ProtocolLocation

trait Resolver { 
  def getAllLocations(clazz :Class[_]) :Seq[ProtocolLocation]
  def getID(location :ProtocolLocation) :String
}

object Resolver { 
  var theResolver :Option[Resolver] = None
  def getResolver :Resolver = theResolver.get
  def setResolver(r :Resolver) { 
    theResolver match { 
      case Some(r1) =>
	if(r1 != r)
	  throw new Exception("Resolver already set!")
      case None => theResolver = Some(r)
    }
  }

  def getAllLocations(clazz :Class[_]) :Seq[ProtocolLocation]= { 
    getResolver.getAllLocations(clazz) 
  }
  
  def getID(location :ProtocolLocation) :String = { 
    getResolver.getID(location)
  }
  
  def getLocation(id :String, clazz :Class[_]) :Option[ProtocolLocation] = { 
    getAllLocations(clazz).find(getID(_)==id)
  }

}


