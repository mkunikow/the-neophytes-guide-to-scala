package parts

import common.printer.printHeader

/**
 * Created by michal on 12/13/14.
 * Part 3: Patterns Everywhere
 */
object part3  extends App {

  //Pattern matching expressions
  {
    printHeader("Pattern matching expressions")

    case class Player(name: String, score: Int)

    def message(player: Player) = player match {
      case Player(_, score) if score > 100000 => "Get a job, dude!"
      case Player(name, _) => "Hey " + name + ", nice to see you again!"
    }

    def printMessage(player: Player) = println(message(player))

    printMessage(Player("Gringo", 10))

  }
  {
    printHeader("Patterns in value definitions")

    case class Player(name: String, score: Int)

    def doSomethingWithTheName(name: String) = println(s"Player name: $name")

    def currentPlayer(): Player = Player("Daniel", 3500)
    val player = currentPlayer()
    doSomethingWithTheName(player.name)

    val Player(pname, _) = currentPlayer()
    doSomethingWithTheName(pname)

    def scores: List[Int] = List()
    //    val best :: rest = scores
    //    println("The score of our champion is " + best) //error pattern doesn't match -> empty list

    def gameResult(): (String, Int) = ("Daniel", 3500)
    val result = gameResult()
    println(result._1 + ": " + result._2)

    //more readable
    val (name, score) = gameResult()
    println(name + ": " + score)

  }
  {
    printHeader("Patterns in for comprehensions")

    def gameResults(): Seq[(String, Int)] =
      ("Daniel", 3500) :: ("Melissa", 13000) :: ("John", 7000) :: Nil

    def hallOfFame = for {
      result <- gameResults()
      (name, score) = result
      if (score > 5000)
    } yield name

    println(s"hallOfFame: $hallOfFame")

    def hallOfFame2 = for {
      (name, score) <- gameResults()
      if (score > 5000)
    } yield name

    println(s"hallOfFame2: $hallOfFame2")

    val lists = List(1, 2, 3) :: List.empty :: List(5, 3) :: Nil

    val listSizes = for {
      list @ head :: _ <- lists
    } yield list.size

    println(s"list sizes without empty list: $listSizes")
  }
}
