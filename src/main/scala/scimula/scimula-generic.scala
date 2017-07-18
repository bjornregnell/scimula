package scimula.generic

case class Event[I, O](
  input:   Option[I]              = None,
  time:    Time                   = Time(0.0),
  handler: Option[Processor[I,O]] = None,
  issuer:  Option[Processor[I,O]] = None)
object Event {
  def apply[I](input: I): Event[I, Nothing] = new Event(Some(input))
}

case class Time(time: Double) extends AnyVal{
  def +(delta: Time): Time = Time(time + delta.time)
}

trait Processor[I,O] { def process: PartialFunction[Event[I, O], O] }
case object Empty extends Processor[Nothing, Unit] { def process = { case _ => } }

class ExecutionContext[I, O] {
  val eventQueue = collection.mutable.PriorityQueue.empty[Event[I, O]](
    Ordering.fromLessThan[Event[I, O]]((e1, e2) => e1.time.time > e2.time.time)
  )

  val outputBuffer = collection.mutable.ListBuffer.empty[O]

  var current = Time(0.0)

  def add(e: Event[I, O]): Unit =
    if (e.time.time >= current.time) eventQueue.enqueue(e)
    else throw new IllegalArgumentException(s"event before current.time: $e")

  def executeNext(): Option[O] = {
    val event = eventQueue.dequeue
    current = event.time
    event.handler.map(h => h.process(event))
  }

  def executeUntil(endOf: Time): Unit =
    while (current.time < endOf.time && eventQueue.size > 0)
      executeNext() foreach (output => outputBuffer prepend output)

  def init(initEvent: Event[I, O]): Unit = {
    eventQueue.clear()
    outputBuffer.clear()
    current = Time(0.0)
    add(initEvent)
  }
}
