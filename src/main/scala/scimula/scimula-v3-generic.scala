package scimula.v3  //scamula

case class Time(time: Double) extends Ordered[Time] {
  def +(delta: Time): Time = Time(time + delta.time)
  def compare(that: Time): Int = (this.time - that.time).toInt
}

case class Event[A](
  data: A,
  at:  Time = Time(0.0),
  handler: Handler[A] = Handler.empty,
  issuer:  Handler[A] = Handler.empty
)

trait Handler[A] { def handle: PartialFunction[Event[A], Unit] }
object Handler {
  def empty[A]: Handler[A] = new Handler[A] { def handle = {case  _ => }}
}

class Executor[A] {

  /*private*/ val eventQueue = collection.mutable.PriorityQueue.empty[Event[A]](
    Ordering.fromLessThan[Event[A]]((e1, e2) => e1.at.time > e2.at.time)
  )

  def events: Stream[Event[A]] = eventQueue.toStream
  def queueLength = eventQueue.length
  def hasNext: Boolean = queueLength > 0

  /*private*/ var current = Time(0.0)
  def now: Time = current

  def add(e: Event[A]): Unit =
    if (e.at.time >= current.time) eventQueue.enqueue(e)  // should this be checked???
    else throw new IllegalArgumentException(s"event before current.time: $e")

  def dispatchNextEvent(): Unit = {
    val event = eventQueue.dequeue
    current = event.at
    event.handler.handle(event)
  }

  def until(endOf: Time): Unit =
    while (current.time < endOf.time && hasNext) dispatchNextEvent()

  def reset(): Unit = {
    eventQueue.clear()
    current = Time(0.0)
  }
}
