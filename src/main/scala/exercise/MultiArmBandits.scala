package exercise

import scala.util.Random

object MultiArmBandits extends App {
  implicit class RichRandom(rand : Random) {
    def nextNormalDistribution(mean : Double, variance : Double) : Double = {
      (random.nextGaussian() * variance) + mean
    }
  }

  val random = new Random()

  def spin(prob : Double) : Boolean =  random.nextDouble() < prob

  trait Arm {
    def step : Unit
    def realValue : Double
    def play : Double
  }

  class StationaryArm(mean : Double) extends Arm {
    override def step : Unit = {}
    override def realValue: Double = mean
    override def play : Double = random.nextNormalDistribution(mean, 1)
  }

  class NonStationaryArm(var mean : Double) extends Arm {
    override def step : Unit = mean += random.nextNormalDistribution(0, 0.01)
    override def realValue: Double = mean
    override def play : Double = random.nextNormalDistribution(mean, 1)
  }

  def generateStationaryKArms(arms : Int) : Seq[Arm] = (1 to 10)
    .map(_ => random.nextNormalDistribution(0, 1))
    .map(new StationaryArm(_))

  def generateNonStationaryKArms(arms : Int) : Seq[Arm] = (1 to 10)
    .map(_ => random.nextNormalDistribution(0, 1))
    .map(new NonStationaryArm(_))

  val runSize = 1000
  val experimentSize = 2000

  type ActionValue = Map[Arm, Double]

  def takeRandom[E, B](elements : Map[E, B]) : (E, B) = elements.toSeq(random.nextInt(elements.size))

  def greedy(actionValueMap: ActionValue) : (Arm, Double) = {
    val max = actionValueMap.maxBy(_._2)._2
    val similar = actionValueMap.filter(_._2 == max)
    takeRandom(similar)
  }

  def epsilonGreedy(epsilon : Double)(actionValueMap: ActionValue) : (Arm, Double) = if(spin(epsilon)) {
    takeRandom(actionValueMap)
  } else {
    greedy(actionValueMap)
  }

  var rewardChoosed = Array.fill(runSize)(0.0)
  val k = 10

  def playWith(arms : Seq[Arm], arm : Arm) : Double = arms.find(arm == _).get.play

  def experiment(run : => Seq[Double]): Seq[Double] = {
    val runs = (1 to experimentSize).map(_ => run)
    (0 until runSize)
      .map(i => runs.map(run => run(i)))
      .map(list => list.sum)
  }

  def run(selection : (ActionValue) => (Arm, Double), armGeneration : => Seq[Arm]) : Seq[Double] = {
    val arms = armGeneration
    var actionValueMap : ActionValue = arms.map(_ -> 0.0).toMap
    var armChosenCount : Map[Arm, Int] = arms.map(_ -> 0).toMap
    val rewards = for {
      _ <- 1 to runSize
      (chosenArm, armEval) = selection(actionValueMap)
      reward = playWith(arms, chosenArm)
      sideEffects = {
        arms.foreach(_.step)
        armChosenCount += chosenArm -> (armChosenCount(chosenArm) + 1)
        actionValueMap += chosenArm -> (armEval + (1 / armChosenCount(chosenArm)) * (reward - armEval))
      }
    } yield (reward)

    rewards.toArray
  }

  val greedyResult = experiment(run(greedy, generateStationaryKArms(k)))
  val epsilonGreedyResult = experiment(run(epsilonGreedy(0.1), generateStationaryKArms(k)))
  val epsilonGreedyLesserResult = experiment(run(epsilonGreedy(0.01), generateStationaryKArms(k)))

  val greedyNonStatinaryResult = experiment(run(greedy, generateNonStationaryKArms(k)))
  val epsilonGreedyNonStatinaryLesserResult = experiment(run(epsilonGreedy(0.01), generateNonStationaryKArms(k)))

  def normalize(seq : Seq[Double]) = seq.map(_ / experimentSize)
  import org.nspl._
  import awtrenderer._

  val plot = xyplot(
    normalize(greedyResult) -> line(color = Color.red, stroke = Stroke(1)),
    normalize(epsilonGreedyResult) -> line(color = Color.blue, stroke = Stroke(1)),
    normalize(epsilonGreedyLesserResult) -> line(color = Color.green, stroke = Stroke(1))
  )()

  val plotNonStationary = xyplot(
    normalize(greedyNonStatinaryResult) -> line(color = Color.red, stroke = Stroke(1)),
    normalize(epsilonGreedyNonStatinaryLesserResult) -> line(color = Color.blue, stroke = Stroke(1))
  )()

  show(plot)
  show(plotNonStationary)
}
