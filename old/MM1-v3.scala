package examples.v3

import scimula._
import v3._

class MM1(val lambda: Double, val mu: Double) {

  trait Message
  case object Generate extends Message
  case object NewJob   extends Message
  case object JobDone  extends Message

  def interarrivalTime = Time(RNG.negExp(lambda))
  def serviceTime      = Time(RNG.negExp(mu))

  val ctx = new ExecutionContext[Message, Unit]

  val jobGenerator = new Processor[Message, Unit] {
    def process = {
      case Event(Generate, now, _, _) =>
        println(s"Generator activated at $now.")
        ctx add Event(NewJob,   at = now, handler = theServer, this)
        ctx add Event(Generate, at = now + interarrivalTime, this, this)
      }
  }

  val theServer = new Processor[Message, Unit] {
    var nbrOfJobsInQ = 0

    def process = {
      case Event(NewJob, now, me, _) =>
        println(s"Server starts processing NewJob at $now. In queue: $nbrOfJobsInQ")
        if (nbrOfJobsInQ == 0)
          ctx add Event(JobDone, at = now + serviceTime, handler = me, issuer = me)
        nbrOfJobsInQ += 1

      case Event(JobDone, now, me, _) =>
        nbrOfJobsInQ -= 1
        println(s"Server JobDone at $now. In queue: $nbrOfJobsInQ")
        if (nbrOfJobsInQ > 0)
          ctx add Event(JobDone, at = now + serviceTime, handler = me, issuer = me)
    }
  }

  def simulate: Unit = {
    ctx.init(Event(Generate, handler = jobGenerator))
    ctx.executeUntil(Time(100.0))
  }
}
