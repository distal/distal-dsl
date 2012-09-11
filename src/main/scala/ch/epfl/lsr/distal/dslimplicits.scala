package ch.epfl.lsr.distal

import ch.epfl.lsr.netty.protocol.ProtocolLocation

trait DSLImplicits { 
  import language.implicitConversions
  implicit def ___string2locationFromConfig(s :String) :ProtocolLocation = DSLProtocol.locationFromConfig(s)
}
