package ch.epfl.lsr.distal

import ch.epfl.lsr.protocol.ProtocolLocation

import scala.collection.mutable.{ UnrolledBuffer, ArrayBuffer, MultiMap, HashMap, Set, Map }

trait DSLWithRuntime {
  val __runtime :DSLRuntime
}

trait DSLRuntime extends DSLRuntimeForEvents {

  private val triggeredEvents = UnrolledBuffer[MessageEvent[_]]()
  private val compositeEvents = UnrolledBuffer[CompositeEvent[_]]()
  private var messages = new HashMap[Message, ArrayBuffer[ProtocolLocation]]

  def onMessageReceived(anyMsg :Any, remoteLocation :ProtocolLocation)

  def addTriggeredEvent(e :MessageEvent[_]) {
    triggeredEvents += e
  }

  def addCompositeEvent(e :CompositeEvent[_]) {
    compositeEvents += e
  }

  def storeMessage(msg :Message, remoteLocation :ProtocolLocation) {
    messages.get(msg) match {
      case None =>
        val ab = new ArrayBuffer[ProtocolLocation]
        ab += remoteLocation
        messages(msg) = ab
      case Some(seq) =>
        seq += remoteLocation
    }

    // val sz = messages.size
    // if(sz > 40) {
    //   println("messages.size=" + messages.size)
    //   if(sz < 45) {
    //     println("BEGIN messages" + messages.size)
    //     messages.keySet.foreach(msg => println(msg))
    //     println("END messages" + messages.size)
    //   }
    // }


  }

  def removeMessages(msgs :Seq[Message]) {
    messages --= msgs
  }

  def removeMessage(msg :Message) {
    messages -= msg
  }

  def executeCompositeEvents(lastMessage :Message) {
    //val start = System.currentTimeMillis
    compositeEvents.foreach {
      _.checkAndExecute(messages, lastMessage, this)
    }
    //println(compositeEvents.size+" composite events took "+ (System.currentTimeMillis - start) +"millis")
  }

  def executeTriggeredEvents(msg :Message, sender :ProtocolLocation) {
    triggeredEvents.foreach {
      _.checkAndExecute(msg, sender, this)
    }
  }

   def reapplyStoredMessages() {
     if(true) {
       val oldMessages = messages
       messages = new HashMap[Message, ArrayBuffer[ProtocolLocation]]

       // simpler version. really deliver all messages again.
       for {
	 msgsenderspair <- oldMessages
	 msg = msgsenderspair._1
	 sender <- msgsenderspair._2
       } onMessageReceived(msg, sender)
     } else {
     /* triggered events: match against all rules */
       for {
	 pair <- messages
	 msg = pair._1
	 sender <- pair._2
	 e <- triggeredEvents
       } e.checkAndExecute(msg, sender, this)

       /* composite events: */
       val messagesForComposition = for {
	 pair <- messages.toSeq
	 sender <- pair._2
       } yield (pair._1,sender)

       compositeEvents.foreach {
	 _.composeAndApplyMessageSets(messagesForComposition, this)
       }
     }
  }



}
