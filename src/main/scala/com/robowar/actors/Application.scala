package com.robowar.actors

import akka.actor.{Actor, ActorLogging}
import scala.concurrent.duration._

class Application extends Actor with ActorLogging {

  import context.dispatcher

  override def preStart(): Unit = {
    log.info("starting")
    context.system.scheduler.scheduleOnce(30.seconds) {
      log.info("sending 'done'")
      self ! "done"
    }
  }

  val receive: Receive = {
    case "done" ⇒
      log.info("received 'done'")
      context.stop(self)
  }

}
