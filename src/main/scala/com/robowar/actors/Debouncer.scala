package com.robowar.actors

import akka.actor.{FSM, Props}
import scala.concurrent.duration.FiniteDuration
import scala.reflect.ClassTag
import Debouncer._

object Debouncer {
  sealed trait State
  case object Idle extends State
  case object Waiting extends State
  case object Queuing extends State

  def props[In: ClassTag, Out](debounceDuration: FiniteDuration,
                               empty: Out,
                               accrue: (Out, In) ⇒ Out,
                               onDebounced: Out ⇒ Unit
                              ): Props = Props(new Debouncer[In, Out](debounceDuration, empty, accrue, onDebounced))
}

class Debouncer[In: ClassTag, Out](debounceDuration: FiniteDuration,
                                   empty: Out,
                                   accrue: (Out, In) ⇒ Out,
                                   onDebounced: Out ⇒ Unit
                                  ) extends FSM[State, Out] {

  startWith(Idle, empty)

  when(Idle) {
    case Event(evt: In, _) =>
      onDebounced(accrue(empty, evt))
      goto(Waiting) using empty
  }

  when(Waiting) {
    case Event('Flush, _) => goto(Idle)
    case Event(evt: In, data) => goto(Queuing) using accrue(empty, evt)
  }

  when(Queuing) {
    case Event('Flush, _) => goto(Idle)
    case Event(evt: In, data) => stay using accrue(data, evt)
  }


  onTransition {
    case Idle -> Waiting => setTimer("flush", 'Flush, debounceDuration, repeat = false)
    case Queuing -> Idle => onDebounced(stateData)
  }

  initialize()
}
