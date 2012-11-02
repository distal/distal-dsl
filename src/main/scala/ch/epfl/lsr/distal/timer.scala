package ch.epfl.lsr.distal

import ch.epfl.lsr.netty.util.{ Timer => TimerImpl }
import java.util.concurrent.TimeUnit
import ch.epfl.lsr.protocol._

case class Duration(amount :Int, unit :TimeUnit)

class ProtoDuration(amount :Int) { 
  def apply(unit :TimeUnit) = Duration(amount,unit)
}

trait ImplicitDurations { 
  import language.implicitConversions
  
  implicit def int2ProtoDuration(i :Int) = new ProtoDuration(i)
}

// TIMER is implemented using AFTER keyword in DSL.
