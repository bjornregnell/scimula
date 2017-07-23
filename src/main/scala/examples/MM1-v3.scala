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

  object exec extends Executor[Message]

  object jobGenerator extends Handler[Message] {
    def handle = {
      case Event(Generate, now, _, _) =>
        println(s"Generator activated at $now.")
        exec add Event(NewJob,   at = now, handler = theServer, this)
        exec add Event(Generate, at = now + interarrivalTime, this, this)
      }

     def start(at: Time): Unit = exec add Event(Generate, at, this, this)
  }

  object theServer extends Handler[Message] {
    var nbrOfJobsInQ = 0

    def handle = {
      case Event(NewJob, now, me, _) =>
        println(s"Server starts processing NewJob at $now. In queue: $nbrOfJobsInQ")
        if (nbrOfJobsInQ == 0)
          exec add Event(JobDone, at = now + serviceTime, handler = me, issuer = me)
        nbrOfJobsInQ += 1

      case Event(JobDone, now, me, _) =>
        nbrOfJobsInQ -= 1
        println(s"Server JobDone at $now. In queue: $nbrOfJobsInQ")
        if (nbrOfJobsInQ > 0)
          exec add Event(JobDone, at = now + serviceTime, handler = me, issuer = me)
    }
  }

  def simulate: Unit = {
    jobGenerator start Time(0.0)
    exec until Time(100.0)
  }
}

object Main {
  def main(args: Array[String]): Unit = new MM1(lambda= 10.0, mu= 15.0).simulate
}
