package exercise

import scala.io.StdIn
import scala.util.Random

object TicTacToe extends App {
  val random = new Random()

  sealed trait Player

  sealed trait Playable extends Player {
    def other : Playable
  }

  case object X extends Playable {
    override val other = O
  }

  case object O extends Playable {
    override val other = X
  }

  case object None extends Player {
    override def toString: String = "_"
  }

  case class Cell(row : Int, column : Int)

  type Game = Map[Cell, Player]

  type ValueBoard = Map[Game, Double]

  val myPlayer = X

  def winning(game : Game, player : Player) = {
    def winWith(cell1 : Cell, cell2: Cell, cell3 : Cell) : Boolean = (game(cell1), game(cell2), game(cell3)) match {
      case (`player`, `player`, `player`) => true
      case _ => false
    }
    def diagonalWin : Boolean = winWith(Cell(0,0), Cell(1,1), Cell(2,2))
    def inversalDiagonalWin : Boolean = winWith(Cell(2,0), Cell(1,1), Cell(0, 2))
    def rowsWin : Boolean = (0 to 2).exists(row => winWith(Cell(row, 0),Cell(row, 1),Cell(row, 2)))
    def columnWin : Boolean = (0 to 2).exists(column => winWith(Cell(0, column),Cell(1, column),Cell(2, column)))

    diagonalWin || inversalDiagonalWin || rowsWin || columnWin
  }

  def isOver(game : Game) : Boolean = game.values.forall(_ != None)

  def odds(game : Game) : Boolean = isOver(game) && (! winning(game, X) || ! winning(game, O))

  val emptyBoard : Seq[Cell] = for {
    row <- 0 to 2
    column <- 0 to 2
  } yield Cell(row, column)

  val emptyGame : Game = emptyBoard.map(_ -> None).toMap

  def prettyPrint(game : Game) : Unit = {
    def rowToString(row : Int) : String = (0 to 2).map(Cell(row , _)).map(game(_)).mkString("|", " ", "|")
    println((0 to 2).map(rowToString).mkString("\n"))
  }
  //ops
  def allFreeMovesFrom(game : Game) : Seq[Cell] = for {
    (cell, player) <- game.toSeq
    if(player == None)
  } yield (cell)

  def nextGames(game : Game, player : Player) : Seq[Game] = for {
    cell <- allFreeMovesFrom(game)
  } yield (game + (cell -> player))

  def allGamesFrom(game : Game) : Set[Game] = {
    def _allGames(player : Playable, game : Game) : Set[Game] = {
      val games = nextGames(game, player).toSet
      games ++ games.flatMap(_allGames(player.other, _))
    }
    _allGames(X, game) ++ _allGames(O, game) + game
  }

  println("compute all games..")
  var valueBoard = allGamesFrom(emptyGame)
    .map(game => game -> winning(game, myPlayer))
    .map {
      case (game, true) => game -> 1.0
      case (game, false) if(isOver(game)) => game -> 0.0
      case (game, false) => game -> 0.5
    }
    .toMap

  def pickRandom(games : Seq[Game]) : Game = {
    games(random.nextInt(games.size))
  }

  def spin(probability : Double) : Boolean = random.nextDouble() < probability

  def greedyChoice(game : Game) : (Game, Double) = {
    nextGames(game, myPlayer)
      .map(game => game -> valueBoard(game))
      .maxBy(_._2)
  }

  def learn(iteration : Int, alpha : => Double, randomProb : => Double) : Unit = for (i <- 0 to iteration) {
    var currentGame : Game = emptyGame
    var currentPlayer : Playable = myPlayer
    val currentAlpha = alpha
    var gamesPlayed : List[Game] = List.empty
    while(!winning(currentGame, myPlayer) && !winning(currentGame, myPlayer.other) && !isOver(currentGame)) {
      gamesPlayed = currentGame :: gamesPlayed
      if (spin(randomProb) || currentGame == emptyGame) {
        currentGame = pickRandom(nextGames(currentGame, currentPlayer))
      } else {
        val bestMove = greedyChoice(currentGame)
        val lastPlayed = gamesPlayed.tail.head
        val currentValue : Double = valueBoard(currentGame)
        valueBoard += lastPlayed -> ((currentValue) + (currentAlpha * (bestMove._2 - currentValue)))
        currentGame = bestMove._1
      }
      currentPlayer = currentPlayer.other
    }
    print(".")
    if(i % 100 == 0) {
      println("")
    }
  }

  def play(times : Int) : Unit = {
    var oddsPlay = 0
    var winningPlay = 0
    for(_ <- 1 to times) {
      var currentGame: Game = emptyGame
      var currentPlayer: Playable = myPlayer
      while (!winning(currentGame, myPlayer) && !winning(currentGame, myPlayer.other) && !isOver(currentGame)) {
        currentGame = greedyChoice(currentGame)._1
        currentPlayer = currentPlayer.other
      }
      if (winning(currentGame, myPlayer)) {
        winningPlay += 1
      }
      if (odds(currentGame)) {
        oddsPlay += 1
      }
    }

    println(s"statistics: winning ${winningPlay}, odds : ${oddsPlay}, loose : ${times - (winningPlay + oddsPlay)}")
  }
  var alpha = 0.5
  val iteration = 100000
  learn(iteration, alpha, 0.1)
  println("")
  println("end train...")
  play(1000)
}
