package scimula

case class Time(time: Double) extends AnyVal{  // Pseudo timestamp or duration
  def +(d: Time): Time = Time(time + d.time)
}

trait Message
case class Text(msg: String) extends Message
case object EmptyMessage     extends Message

case class Event(msg: Message, time: Time, handler: Processor, issuer: Processor)

object Event {
  val timeOrdering = Ordering.fromLessThan[Event]((e1, e2) => e1.time.time > e2.time.time)
}

trait Processor { def process: PartialFunction[Event, Unit] }
case object NoProcess extends Processor { def process = { case _ => } }

class Simulation {
  val eventQueue = collection.mutable.PriorityQueue.empty[Event](Event.timeOrdering)

  val log = collection.mutable.ListBuffer.empty[Any]

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
