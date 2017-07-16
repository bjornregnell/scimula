package scimula

object RNG { // Random Number Generator
  val rng = java.util.concurrent.ThreadLocalRandom.current
  def negExp(mean: Double)        = math.log(1-rng.nextDouble())/(-1/mean)
  def rect(from: Int, until: Int) = rng.nextInt(from, until)
  def setSeed(seed: Long)         = rng.setSeed(seed)
}
