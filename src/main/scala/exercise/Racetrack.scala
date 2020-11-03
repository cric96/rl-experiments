package exercise

import java.awt.geom.Line2D
import java.util.concurrent.{Executors, TimeUnit}

import javafx.application.{Application, Platform}
import javafx.scene.Scene
import javafx.scene.control.Button
import javafx.scene.layout.{Border, BorderStroke, BorderStrokeStyle, BorderWidths, CornerRadii, GridPane}
import javafx.stage.Stage
import javafx.embed.swing.JFXPanel
import javafx.geometry.Insets
import javafx.scene.paint.Color
import javafx.scene.shape.Rectangle
import javafx.scene.text.Font

import scala.util.Random

object Racetrack extends App {
  val random = new Random()

  implicit class RichSeq[T](seq : Seq[T]) {
    def randomElement : T = seq(random.nextInt(seq.size))
  }
  type Point = (Int, Int)

  implicit class RichPoint(point: Point) {
    def +(p: Point): Point = (point, p) match {
      case ((x, y), (k, z)) => (x + k, y + z)
    }

    def -(p: Point): Point = (point, p) match {
      case ((x, y), (k, z)) => (x - k, y - z)
    }
  }

  trait Line {
    def start : Point
    def end : Point
    def intersect(other : Line) : Boolean = (this.start, this.end, other.start, other.end) match {
      case ((x1, y1), (x2, y2), (x3, y3), (x4, y4)) => Line2D.linesIntersect(x1, y1, x2, y2, x3, y3, x4, y4)
    }
  }

  case class EndStartLine(start : Point, end : Point) extends Line

  trait PointableLines extends Line {
    def points : Seq[Point]
  }
  case class HorizontalLine(start : Point, length : Int) extends PointableLines {
    override val end: Point = start match {
      case (x, y) => (x + length, y)
    }

    override val points: Seq[(Int, Int)] = (start, end) match {
      case ((startX, y), (endX, _)) => (startX to endX).map(x => (x, y))
    }
  }

  case class VerticalLine(start : Point, length : Int) extends PointableLines {
    override val end: (Int, Int) = start match {
      case (x, y) => (x, y + length)
    }

    override val points: Seq[(Int, Int)] = (start, end) match {
      case ((x, startY), (_, endY)) => (startY to endY).map(y => (x, y))
    }
  }

  case class Arena(columns : Int, rows : Int, start : PointableLines, end : PointableLines) {
    val bounds = List(
      EndStartLine((0,0),(rows, 0)),
      EndStartLine((0,0),(0, columns)),
      EndStartLine((rows, 0), (rows, columns)),
      EndStartLine((0, columns),(rows, columns))
    )
    def hitBound(trajectory : Line) : Boolean = bounds.exists(_.intersect(trajectory))
    def hitEnd(trajectory : Line) : Boolean = end.intersect(trajectory)
  }

  type Velocity = (Int, Int)
  type Player = (Point, Velocity)
  type Policy = Player => Velocity
  type Reward = Double
  type SAR = (Player, Velocity, Reward)

  implicit class RichSAR(sar : SAR) {
    def state : Player = sar._1
    def action : Velocity = sar._2
    def reward : Reward = sar._3
  }
  implicit class RichPlayer(p : Player) {
    def position = p._1
    def velocity = p._2
    def update(velocity: Velocity) : Player = (trajectory(velocity).end, acceleration(velocity))
    def trajectory(velocity: Velocity) : Line = {
      val newVelocity = velocity + this.velocity
      EndStartLine(position, position + newVelocity)
    }
    def acceleration(velocity : Velocity) : Velocity = this.velocity + velocity
  }
  def randomStartPosition(arena : Arena) : Point = {
    arena.start.points.randomElement
  }

  def p(position : Point, velocity: Velocity) : Player = (position, velocity)

  object Velocity {
    val Zero : Velocity = (0,0)
  }
  object Reward {
    val Bad : Reward = -1
    val Good : Reward = 0
  }

  def move(arena : Arena, player : Player, policy: Policy) : SAR = {
    val velocity = policy(player)
    val trajectory = player.trajectory(velocity)
    if(arena.hitBound(trajectory)) {
      println("HIT BOUND!")
      (p(randomStartPosition(arena), Velocity.Zero), velocity, Reward.Bad)
    } else if(arena.hitEnd(trajectory)) {
      println("HIT END!")
      (player, velocity, Reward.Good)
    } else {
      println("GO ON...")
      (player.update(velocity), velocity, Reward.Bad)
    }
  }

  val maxVelocityCoord = 5

  def eligible(move : Int, coord : Int) : Boolean = move + coord match {
    case n if n < 0 => false
    case n if n > 5 => false
    case _ => true
  }

  def randomPolicy(player : Player) : Velocity = {
    val upDownEligible = (-1 to 1).filter(move => eligible(move, player.velocity._1))
    val rightLeftEligible = (-1 to 1).filter(move => eligible(move, player.velocity._2))
    println(player)
    println(upDownEligible)
    println(rightLeftEligible)
    (for {
      upDown <- upDownEligible
      rightLeft <- rightLeftEligible
    } yield (upDown, rightLeft)).randomElement
  }

  class ArenaGUI(arena: Arena) {
    val playerStroke = new BorderStroke(Color.BLACK, BorderStrokeStyle.SOLID, CornerRadii.EMPTY, new BorderWidths(1))
    val noBorder = Border.EMPTY
    val playerBorder = new Border(playerStroke)
    val pane = new GridPane
    pane.setPadding(new Insets(10))
    pane.setHgap(10)
    pane.setVgap(10)
    val representation = (for {
      row <- 0 until arena.rows;
      column <- 0 until arena.columns
    } yield ((row, column) -> new Button(" "))).toMap
    representation.foreach { case (_, button) => button.setMinWidth(40)}
    representation.foreach { case (_, button) => button.setTextFill(Color.RED)}
    representation.foreach { case (_, button) => button.setFont(Font.font(20))}
    representation.foreach { case (_, button) => button.setDisable(true)}
    representation.foreach { case (_, button) => button.setStyle("-fx-background-color: #919191 ")}
    representation.foreach { case ((row, column), button) => pane.add(button, column, row)}
    arena.start.points.map(representation).foreach(button => button.setStyle("-fx-background-color: #9eff78 "))
    arena.end.points.map(representation).foreach(button => button.setStyle("-fx-background-color: #ffac78"))
    var oldPlayer : Option[Player] = None
    def renderPlayer(p : Player) : Unit = {
      oldPlayer match {
        case Some(p) => representation(p.position).setText("")
          representation(p.position).setBorder(noBorder)
        case _ =>
      }
      representation(p.position).setText("*")
      representation(p.position).setBorder(playerBorder)
      oldPlayer = Some(p)
    }
  }

  def sequence(policy : Policy, arena : Arena) : Seq[SAR] = {
    var player : Player = p(randomStartPosition(arena), Velocity.Zero)
    var last : SAR = null
    val result = Stream.continually(arena)
      .map(arena => {
        val update : SAR = move(arena, player, policy)
        player = update.state
        last = update
        update
      })
      .takeWhile(_.reward != Reward.Good)
      .toList
    result :+ last
  }
  class Grid() extends Application {
    override def start(primaryStage: Stage): Unit = {
      primaryStage.setTitle("game")
      val arena = Arena(10, 10, HorizontalLine((1, 2), 3), VerticalLine((5, 5), 2))
      val gui = new ArenaGUI(arena)
      var player = p(randomStartPosition(arena), (0,0))
      gui.renderPlayer(player)
      val scene = new Scene(gui.pane, 800, 600)
      primaryStage.setScene(scene)
      val moves = sequence(randomPolicy, arena)
      Executors.newSingleThreadExecutor().submit(() => {
        
      })
      moves.foreach {

        Thread.sleep(100)
      }
      /*Executors.newSingleThreadScheduledExecutor().scheduleAtFixedRate(
        () => {
          val (newPlayer, velocity, reward) = move(arena, player, randomPolicy)
          player = newPlayer
          if(reward == 0){
            System.exit(0)
          }
          Platform.runLater(() => gui.renderPlayer(player))
        },
        0,
        100,
        TimeUnit.MILLISECONDS
      )*/
      primaryStage.show()
    }
  }
  Application.launch(classOf[Grid])
}
