package scimula.v2
// no generics, explicit base type Tag, with processor result stored in output

case class Event(
  tag: Tag  = NoTag,
  at:  Time = Time(0.0),
  handler: Processor = EmptyProcessor,
  issuer:  Processor = EmptyProcessor)

case class Time(time: Double) extends AnyVal with Ordered[Time] {
  def +(delta: Time): Time = Time(time + delta.time)
  def compare(that: Time): Int = (this.time - that.time).toInt
}

trait Tag
case class  Data[T](tag: T)      extends Tag
case class  Message(tag: String) extends Tag
case object NoTag                extends Tag

trait Output
case class  Result[T](output: T) extends Output
case object NoResult             extends Output

trait Processor { def process: PartialFunction[Event, Output] }
case object EmptyProcessor extends Processor { def process = { case _ =>  NoResult } }

class ExecutionContext {
  /*private*/ val eventQueue = collection.mutable.PriorityQueue.empty[Event](
    Ordering.fromLessThan[Event]((e1, e2) => e1.at.time > e2.at.time)
  )

  def events: Stream[Event] = eventQueue.toStream
  def queueLength = eventQueue.length
  def hasNext: Boolean = queueLength > 0

  /*private*/ val outputBuffer = collection.mutable.ListBuffer.empty[Output]
  def results: List[Output] = outputBuffer.toList

  /*private*/ var current = Time(0.0)
  def now: Time = current

  def add(e: Event): Unit =
    if (e.at.time >= current.time) eventQueue.enqueue(e)
    else throw new IllegalArgumentException(s"event before current.time: $e")

  def executeNext(): Unit = {
    val event = eventQueue.dequeue
    current = event.at
    val output = event.handler.process(event)
    if (output != NoResult) outputBuffer prepend output
  }

  def executeUntil(endOf: Time): Unit =
    while (current.time < endOf.time && hasNext) executeNext()

  def init(initEvent: Event = Event()): Unit = {
    eventQueue.clear()
    outputBuffer.clear()
    current = Time(0.0)
    add(initEvent)
  }
}
