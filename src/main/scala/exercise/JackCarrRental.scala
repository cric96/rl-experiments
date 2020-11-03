package exercise

import scala.util.Random

object JackCarrRental extends App {
  val random = new Random()

  def factorial(n : Long) : Long = {
    def _factorial(n : Long, acc : Long) : Long = if(n == 0) {
      acc
    } else {
      _factorial(n - 1, acc * n)
    }
    _factorial(n, 1)
  }

  def poisson(lambda : Int, n : Int) : Double = {
    (Math.pow(lambda, n) / factorial(n)) * (Math.pow(Math.E, -lambda))
  }
  def generateInterval(howMany : Int, value : (Int) => Double) : Seq[(Int, Double)] = {
    val keys = 0 to howMany
    val values = keys map { value }
    keys.zip(values)
  }

  val maxCars = 20
  val maximumValueForSpin = 10
  val maxMoving = 5

  val poissonRequestA = generateInterval(maximumValueForSpin, poisson(3, _))
  val poissonRequestB = generateInterval(maximumValueForSpin, poisson(4, _))
  val poissonReturnA = generateInterval(maximumValueForSpin, poisson(3, _))
  val poissonReturnB = generateInterval(maximumValueForSpin, poisson(2, _))

  type State = (Location, Location)
  type Reward = Double
  type Probability = Double
  type Move = Map[State, Seq[(Reward, Probability)]]

  case class CarMoving(rent : Int, returned : Int) {
    def delta : Int = returned - rent
  }

  val rentReward : Reward = 10.0
  val discountedRate : Double = 0.9
  val moveCont : Double = 2

  val allMachinesPossibilities : Seq[(Probability, (CarMoving, CarMoving))] = for {
    (howManyA, probRequestA) <- poissonRequestA
    (howManyB, probRequestB) <- poissonRequestB
    (returnedA, probReturnA) <- poissonReturnA
    (returnedB, probReturnB) <- poissonReturnB
    probability = probRequestA * probRequestB * probReturnA * probReturnB
    deltaA = CarMoving(howManyA, returnedA)
    deltaB = CarMoving(howManyB, returnedB)
  } yield (probability, (deltaA, deltaB))

  case class Location(howMay : Int) {
    def + (delta : Int) : Location = Location(howMay + delta match {
      case n if (n < 0) => 0
      case n if (n > maxCars) => maxCars
      case n => n
    })
    def - (delta : Int) : Location = Location(howMay - delta match {
      case n if (n < 0) => 0
      case n if (n > maxCars) => maxCars
      case n => n
    })

    private def evalMoney(delta : CarMoving) : Reward = {
      val totalMoved = if (howMay - delta.rent >= 0) delta.rent else howMay
      totalMoved * rentReward
    }
    def daySpent(delta : CarMoving) : (Location, Reward) = {
      val reward = evalMoney(delta)
      val rentEval = this - delta.rent
      val returnEval = rentEval + delta.returned
      (returnEval, reward)
    }
  }

  implicit class RichState(state : State) {
    def moveToA(howMany : Int) : State = state match {
      case (locationA, locationB) => (locationA + howMany, locationB - howMany)
    }
    def moveToB(howMany : Int) : State = state match {
      case (locationA, locationB) => (locationA - howMany, locationB + howMany)
    }
    def validMove(howMany : Int) : Boolean = (howMany, state) match {
      case (n, (locA, locB)) if n > 0 => (locA.howMay - n) >= 0
      case (n, (locA, locB)) if n < 0 => (locB.howMay + n) >= 0
      case _ => true
    }
  }

  def movesFromState(state : State) : Move = allMachinesPossibilities
    .par
    .map { case (prob, (deltaA, deltaB)) => (state._1.daySpent(deltaA), state._2.daySpent(deltaB), prob) }
    .map { case ((locationA, rewardA), (locationB, rewardB), prob) => ((locationA, locationB) -> (rewardA + rewardB, prob))}
    .seq
    .groupBy { case (location, payload) => location }
    .mapValues[Seq[(Reward, Probability)]](sequence => sequence.map(_._2))

  println("compute all state and successive...")

  val allState = for {
    a <- (0 to maxCars)
    b <- (0 to maxCars)
  } yield (Location(a), Location(b))

  def prettyPrint[E](states : Map[State, E]): Unit = {
    for (a <- maxCars to 0 by -1) {
      for(b <- 0 to maxCars) {
        print(states(Location(a), Location(b)) + "\t")
      }
      println("")
    }
  }

  val allMovesFromState : Map[State, Move] = (for {
    state <- allState
  } yield (state -> movesFromState(state))).toMap

  var policy : Map[State, (Int)] = allState.map { _ -> (0) }.toMap
  var valueFunction : Map[State, Double] = allState.map {_ -> 0.0 }.toMap

  var stopWhen = 0.1

  def evalPolicy(): Unit = {
    var maxDelta = 0.0
    do {
      maxDelta = 0.0
      for (state <- allState) {
        val v = valueFunction(state)
        val action = policy(state)
        val actionReward = - Math.abs(action) * moveCont
        val updateState = if(action > 0) {
          state.moveToB(action)
        } else {
          state.moveToA(-action)
        }
        val allNextStates = allMovesFromState(updateState)
        val updates = for {
          (nextState, evaluations) <- allNextStates
          (reward, prob) <- evaluations
        } yield (prob * (reward + actionReward + discountedRate * valueFunction(nextState)))
        valueFunction += state -> updates.sum
        maxDelta = Math.max(maxDelta, Math.abs(v - valueFunction(state)))
      }
      println(s"max delta : $maxDelta")
    } while(maxDelta > stopWhen)
  }

  def improvePolicy(): Unit = {
    for (state <- allState) {
      val oldAction = policy(state)
      val actionEvaluation = for {
        action <- (-maxMoving to maxMoving).filter(state.validMove)
        actionReward = - Math.abs(action) * moveCont
        updateState = if (action > 0) {
          state.moveToB(action)
        } else {
          state.moveToA(-action)
        }
        updates = for {
          (nextState, evaluations) <- allMovesFromState(updateState)
          (reward, prob) <- evaluations
        } yield (prob * (reward + actionReward + discountedRate * valueFunction(nextState)))
      } yield (action -> updates.sum)
      val best = actionEvaluation.maxBy(_._2)
      policy += state -> best._1
    }
  }
  while(true) {
    println("Current policy")
    prettyPrint(policy)
    evalPolicy()
    println("Policy evaluation ends...")
    improvePolicy()
    println("Policy improvement ends...")
  }
}
