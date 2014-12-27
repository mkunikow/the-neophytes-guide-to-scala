package parts
import common.printer.printHeader

/**
 * Created by michal on 12/27/14.
 * The Neophyte's Guide to Scala Part 8: Welcome to the Future
 */
object part8 extends App {

  //Why sequential code can be bad
  {
    printHeader("Why sequential code can be bad")

    import scala.util.Try
    // Some type aliases, just for getting more meaningful method signatures:
    type CoffeeBeans = String
    type GroundCoffee = String
    case class Water(temperature: Int)
    type Milk = String
    type FrothedMilk = String
    type Espresso = String
    type Cappuccino = String
    // dummy implementations of the individual steps:
    def grind(beans: CoffeeBeans): GroundCoffee = s"ground coffee of $beans"
    def heatWater(water: Water): Water = water.copy(temperature = 85)
    def frothMilk(milk: Milk): FrothedMilk = s"frothed $milk"
    def brew(coffee: GroundCoffee, heatedWater: Water): Espresso = "espresso"
    def combine(espresso: Espresso, frothedMilk: FrothedMilk): Cappuccino = "cappuccino"
    // some exceptions for things that might go wrong in the individual steps
    // (we'll need some of them later, use the others when experimenting
    // with the code):
    case class GrindingException(msg: String) extends Exception(msg)
    case class FrothingException(msg: String) extends Exception(msg)
    case class WaterBoilingException(msg: String) extends Exception(msg)
    case class BrewingException(msg: String) extends Exception(msg)


    // going through these steps sequentially:
    def prepareCappuccino(): Try[Cappuccino] = for {
      ground <- Try(grind("arabica beans"))
      water <- Try(heatWater(Water(25)))
      espresso <- Try(brew(ground, water))
      foam <- Try(frothMilk("milk"))
    } yield combine(espresso, foam)

    //Working with Futures
    {
     printHeader("Working with Futures")
      import scala.concurrent.Future
      import scala.concurrent.ExecutionContext.Implicits.global
      import scala.util.Random

      def grind(beans: CoffeeBeans): Future[GroundCoffee] = Future {
        println("start grinding...")
        Thread.sleep(Random.nextInt(2000))
        if (beans == "baked beans") throw GrindingException("are you joking?")
        println("finished grinding...")
        s"ground coffee of $beans"
      }

      def heatWater(water: Water): Future[Water] = Future {
        println("heating the water now")
        Thread.sleep(Random.nextInt(2000))
        println("hot, it's hot!")
        water.copy(temperature = 85)
      }

      def frothMilk(milk: Milk): Future[FrothedMilk] = Future {
        println("milk frothing system engaged!")
        Thread.sleep(Random.nextInt(2000))
        println("shutting down milk frothing system")
        s"frothed $milk"
      }

      def brew(coffee: GroundCoffee, heatedWater: Water): Future[Espresso] = Future {
        println("happy brewing :)")
        Thread.sleep(Random.nextInt(2000))
        println("it's brewed!")
        "espresso"
      }

//    definition of future
//      object Future {
//        def apply[T](body: => T)(implicit execctx: ExecutionContext): Future[T]
//      }

      //Callbacks
      {
        printHeader("Callbacks")
        grind("arabica beans").onSuccess { case ground =>
          println("okay, got my ground coffee")
        }

        import scala.util.{Success, Failure}
        grind("baked beans").onComplete {
          case Success(ground) => println(s"got my $ground")
          case Failure(ex) => println("This grinder needs a replacement, seriously!")
        }

      }

      //Composing futures
      {
        printHeader("Composing futures")

        //Mapping the future
        {
          printHeader("Mapping the future")
          val temperatureOkay: Future[Boolean] = heatWater(Water(25)).map { water =>
            println("we're in the future!")
            (80 to 85).contains(water.temperature)
          }
        }
        //Keeping the future flat
        def temperatureOkay(water: Water): Future[Boolean] = Future {
          (80 to 85).contains(water.temperature)
        }

        {
          printHeader("Keeping the future flat")

          //Use flatMap instead of map in order to get a Future[Boolean] instead of a Future[Future[Boolean]]:
          val nestedFuture: Future[Future[Boolean]] = heatWater(Water(25)).map {
            water => temperatureOkay(water)
          }
          val flatFuture: Future[Boolean] = heatWater(Water(25)).flatMap {
            water => temperatureOkay(water)
          }
        }

        //For comprehensions
        {
          printHeader("For comprehensions")
          val acceptable: Future[Boolean] = for {
            heatedWater <- heatWater(Water(25))
            okay <- temperatureOkay(heatedWater)
          } yield okay
        }

//        If you have multiple computations that can be computed in parallel, you need to take care that
//        you already create the corresponding Future instances outside of the for comprehension.

        def prepareCappuccinoSequentially(): Future[Cappuccino] = {
          for {
            ground <- grind("arabica beans")
            water <- heatWater(Water(20))
            foam <- frothMilk("milk")
            espresso <- brew(ground, water)
          } yield combine(espresso, foam)
        }

//        Hence, make sure to instantiate all your independent futures before the for comprehension:
        def prepareCappuccino(): Future[Cappuccino] = {
          val groundCoffee = grind("arabica beans")
          val heatedWater = heatWater(Water(20))
          val frothedMilk = frothMilk("milk")

          for {
            ground <- groundCoffee
            water <- heatedWater
            foam <- frothedMilk
            espresso <- brew(ground, water)
          } yield combine(espresso, foam)
        }
      }
    }
  }
}
