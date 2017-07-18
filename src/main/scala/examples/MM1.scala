import scimula._

class MM1(val lambda: Double, val mu: Double) {
  case object Generate extends Tag
  case object NewJob   extends Tag
  case object JobDone  extends Tag

  def interarrivalTime = Time(RNG.negExp(lambda))
  def serviceTime      = Time(RNG.negExp(mu))

  val ctx = new ExecutionContext

  val jobGenerator = new Processor {
    def process = {
      case Event(Generate, now, me, _) =>
        println(s"Generator activated at $now.")
        ctx add Event(NewJob,   time = now, handler = theServer, issuer = me)
        ctx add Event(Generate, time = now + interarrivalTime, handler = me, issuer = me)
      }
  }

  val theServer = new Processor {
    var nbrOfJobsInQ = 0

    def process = {
      case Event(NewJob, now, me, _) =>
        println(s"Server starts processing NewJob at $now. In queue: $nbrOfJobsInQ")
        if (nbrOfJobsInQ == 0)
          ctx add Event(JobDone, time = now + serviceTime, handler = me, issuer = me)
        nbrOfJobsInQ += 1

      case Event(JobDone, now, me, _) =>
        nbrOfJobsInQ -= 1
        println(s"Server JobDone at $now. In queue: $nbrOfJobsInQ")
        if (nbrOfJobsInQ > 0)
          ctx add Event(JobDone, time = now + serviceTime, handler = me, issuer = me)
    }
  }

  def simulate = {
    ctx init Event(tag = Generate, handler = jobGenerator)
    ctx executeUntil Time(100.0)
  }
}
