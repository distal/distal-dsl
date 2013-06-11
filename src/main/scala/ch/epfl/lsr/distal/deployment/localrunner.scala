package ch.epfl.lsr.distal.deployment

import ch.epfl.lsr.distal._
import ch.epfl.lsr.netty.network.{ ProtocolLocation => NettyProtocolLocation }
import ch.epfl.lsr.protocol.{ ProtocolLocation => ProtocolLocationBase }

import java.net.URI


/** gets ID from path which is assumed to have format "/ID/whatever" */
class StringsResolver(locationStrings :Array[String]) extends Resolver {
  private val locations = locationStrings.map(new NettyProtocolLocation(_))

  def getAllLocations(clazz :Class[_]) :Seq[ProtocolLocationBase] = {
    locations.filter { _.isForClazz(clazz) }
  }

  def getID(location :ProtocolLocationBase) :String = {
    location.asInstanceOf[NettyProtocolLocation].name.split("/")(1)
  }

  def getLocations(ID :String) :Seq[ProtocolLocationBase] = {
    locations.filter(_.name.split("/")(1)==ID)
  }
}

object LocalRunner {
  def main(args :Array[String]) {
    val local_ID = args(0)
    val all_locations = args.drop(1)

    val resolver = new StringsResolver(all_locations)

    Resolver.setResolver(resolver)

    val protocols  = DSLProtocolRunner.createProtocols(local_ID)

    protocols.foreach {
      protocol =>
      println(" "+protocol.getClass)
      protocol.start
    }
    println("done starting")
  }


}
