package scimula

case class Event(
  tag: Tag           = NoTag,
  time: Time         = Time(0.0),
  handler: Processor = Empty,
  issuer: Processor  = Empty)

case class Time(time: Double) extends AnyVal{
  def +(delta: Time): Time = Time(time + delta.time)
}

trait Tag
case class Message(msg: String) extends Tag
case object NoTag               extends Tag

trait Processor { def process: PartialFunction[Event, Unit] }
case object Empty extends Processor { def process = { case _ => } }

class ExecutionContext {
  val eventQueue = collection.mutable.PriorityQueue.empty[Event](
    Ordering.fromLessThan[Event]((e1, e2) => e1.time.time > e2.time.time)
  )

  var current = Time(0.0)

  def add(e: Event): Unit =
    if (e.time.time >= current.time) eventQueue.enqueue(e)
    else throw new IllegalArgumentException(s"event before current.time: $e")

  def executeNext(): Unit = {
    val event = eventQueue.dequeue
    current = event.time
    event.handler.process(event)
  }

  def executeUntil(endOf: Time): Unit =
    while (current.time < endOf.time && eventQueue.size > 0) executeNext()

  def init(initEvent: Event): Unit = {
    eventQueue.clear()
    current = Time(0.0)
    add(initEvent)
  }
}
