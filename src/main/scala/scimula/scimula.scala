package scimula

case class Time(time: Double) extends AnyVal{  // Pseudo timestamp or duration
  def +(delta: Time): Time = Time(time + delta.time)
}

trait EventType
case class Message(msg: String) extends EventType
case object Untyped             extends EventType

case class Event(tpe: EventType, time: Time, handler: Processor, issuer: Processor)

trait Processor { def process: PartialFunction[Event, Unit] }
case object NoProcess extends Processor { def process = { case _ => } }

class Simulation {
  val eventQueue = collection.mutable.PriorityQueue.empty[Event](
    Ordering.fromLessThan[Event]((e1, e2) => e1.time.time > e2.time.time)
  )

  var current = Time(0.0)

  def add(e: Event): Unit =
    if (e.time.time >= current.time) eventQueue.enqueue(e)
    else throw new IllegalArgumentException

  def simulateUntil(endOf: Time): Unit =
    while (current.time < endOf.time && eventQueue.size > 0) {
      val event = eventQueue.dequeue
      current = event.time
      event.handler.process(event)
    }

  def init(initEvent: Event): Unit = {
    eventQueue.clear()
    current = Time(0.0)
    add(initEvent)
  }
}
