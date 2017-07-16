package scimula

object MM1 {
  case object Generate extends Message
  case object NewJob   extends Message
  case object JobDone  extends Message

  def interarrivalTime = RNG.negExpTime(Time(15.0))
  def serviceTime      = RNG.negExpTime(Time(10.0))
  
  val sim = new Simulation
  
  lazy val jobGenerator = new Processor { 
    def process = {
      case Event(Generate, now, me, _) => 
        println(s"Generator activated at $now.")
        sim add Event(NewJob,   time = now, handler = theServer, issuer = me)
        sim add Event(Generate, time = now + interarrivalTime, handler = me, issuer = me)  
      }
  }
  
  lazy val theServer = new Processor { 
    var nbrOfJobsInQ = 0
    
    def process = { 
      case Event(NewJob, now, me, _) =>
        println(s"Server starts processing NewJob at $now. In queue: $nbrOfJobsInQ")
        if (nbrOfJobsInQ == 0) 
          sim add Event(JobDone, time = now + serviceTime, handler = me, issuer = me)
        nbrOfJobsInQ += 1

      case Event(JobDone, now, me, _) =>
        nbrOfJobsInQ -= 1
        println(s"Server JobDone at $now. In queue: $nbrOfJobsInQ")
        if (nbrOfJobsInQ > 0) 
          sim add Event(JobDone, time = now + serviceTime, handler = me, issuer = me)
    }  
  }

  def simulate = {
    sim init Event(Generate, Time(0.0), handler = jobGenerator, issuer = NoProcess)
    sim simulateUntil Time(100.0)
  }  
  
}
