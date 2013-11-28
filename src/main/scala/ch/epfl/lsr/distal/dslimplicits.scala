package ch.epfl.lsr.distal

import ch.epfl.lsr.protocol.ProtocolLocation


class ProtocolLocationWithID(pl :ProtocolLocation) extends ProtocolLocation { 
  val scheme = pl.scheme
  val id = DSLProtocol.idForLocation(pl)
  def /(path :String) = pl/path
}

// TODO ??? 
trait DSLImplicits { 
  self :DSL => 
  import language.implicitConversions
  implicit def ___string2locationFromConfig(s :String) :ProtocolLocation = 
    DSLProtocol.locationForId(self, s).get
  implicit def __location2IDstring(loc :ProtocolLocation) :String = 
    DSLProtocol.idForLocation(loc)
  implicit def __locations2IDstrings(locs :collection.Set[ProtocolLocation]) : collection.Set[String] = 
    locs.map(DSLProtocol.idForLocation(_))
  implicit def __protocol2location(proto :DSLProtocol) : ProtocolLocation = 
    proto.LOCATION
  implicit def __protocols2locations[B <: DSLProtocol](protos :Seq[B]) :Array[ProtocolLocation] = 
    protos.map(_.LOCATION).toArray
  implicit def __locations2locationArray(seq :Seq[ProtocolLocation]) :Array[ProtocolLocation] = 
    seq.toArray

  implicit def __protocolLocation2withID(pl :ProtocolLocation) :ProtocolLocationWithID = 
    new ProtocolLocationWithID(pl)
}
