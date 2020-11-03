package exercise

import exercise.GamblerProblems.valueFunction
import javafx.scene.chart.XYChart.Series

object GamblerProblems extends App {
  type Reward = Double
  type State = Int
  val thr = 0.001
  val discounted = 0.9
  val maxCapital = 100
  val states : Seq[Int] = (0 to maxCapital)
  def actions(s : State) : Seq[Int] = (1 to Math.min(s, maxCapital - s))
  def nonTerminal(s : State) : Boolean = s != 0 && s != maxCapital
  var valueFunction : Map[State, Reward] = states map {_ -> 0.0 } toMap

  //valueFunction += maxCapital -> 1.0

  val ph = 0.9 //probability to spin an head
  def evalAction(action : Int, state : State) : Double = {
    val reward = if(action + state == maxCapital) 1.0 else 0.0
    val win = ph * (reward + valueFunction(action + state) * discounted)
    val loose = (1 - ph) * (valueFunction(state - action) * discounted)
    win + loose
  }
  var maxDelta = 0.0
  var plots : Seq[Seq[Double]] = Seq.empty
  do {
    maxDelta = 0.0
    for (state <- states;
         if nonTerminal(state)) {
      val v = valueFunction(state)
      val bestAction = actions(state).map(action => evalAction(action, state)).max
      valueFunction += state -> bestAction
      maxDelta = Math.max(maxDelta, Math.abs(v - valueFunction(state)))
    }
    plots = plots :+ valueFunction.toSeq.sortBy(_._1).map(_._2)

  } while(maxDelta > thr)
  import org.nspl._
  import org.nspl.data._
  import org.nspl.awtrenderer._
  val plotsWithColor = plots.map(_ -> line(color = Color.red, stroke = Stroke(0.2)))

  val plotValue = xyplot(plotsWithColor:_*)()

  val policy = states.filter(nonTerminal).map {
    case state => actions(state).filter(nonTerminal).map(action => action -> evalAction(action, state)).maxBy(_._2)._1
  }

  val plotPolicy = xyplot(policy.map(_.toDouble) -> point())()

  show(plotPolicy)
  show(plotValue)
}
