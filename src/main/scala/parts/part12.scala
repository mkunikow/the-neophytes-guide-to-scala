package parts
import common.printer.printHeader

/**
 * Created by michal on 1/1/15.
 * The Neophyte's Guide to Scala Part 12: Type Classes
 */
object part12 extends App {

  //The problem
  {
    printHeader("The problem")

    {
      object Statistics {
        def median(xs: Vector[Double]): Double = xs(xs.size / 2)

        def quartiles(xs: Vector[Double]): (Double, Double, Double) =
          (xs(xs.size / 4), median(xs), xs(xs.size / 4 * 3))

        def iqr(xs: Vector[Double]): Double = quartiles(xs) match {
          case (lowerQuartile, _, upperQuartile) => upperQuartile - lowerQuartile
        }

        def mean(xs: Vector[Double]): Double = {
          xs.reduce(_ + _) / xs.size
        }
      }
    }

    {
      object Statistics {
        def median(xs: Vector[Number]): Number = ???
        def quartiles(xs: Vector[Number]): (Number, Number, Number) = ???
        def iqr(xs: Vector[Number]): Number = ???
        def mean(xs: Vector[Number]): Number = ???
      }
    }

    //Adapter pattern
    {
      object Statistics {
        trait NumberLike[A] {
          def get: A
          def plus(y: NumberLike[A]): NumberLike[A]
          def minus(y: NumberLike[A]): NumberLike[A]
          def divide(y: Int): NumberLike[A]
        }

        case class NumberLikeDouble(x: Double) extends NumberLike[Double] {
          def get: Double = x
          def minus(y: NumberLike[Double]) = NumberLikeDouble(x - y.get)
          def plus(y: NumberLike[Double]) = NumberLikeDouble(x + y.get)
          def divide(y: Int) = NumberLikeDouble(x / y)
        }

        type Quartile[A] = (NumberLike[A], NumberLike[A], NumberLike[A])

        def median[A](xs: Vector[NumberLike[A]]): NumberLike[A] = xs(xs.size / 2)

        def quartiles[A](xs: Vector[NumberLike[A]]): Quartile[A] =
          (xs(xs.size / 4), median(xs), xs(xs.size / 4 * 3))

        def iqr[A](xs: Vector[NumberLike[A]]): NumberLike[A] = quartiles(xs) match {
          case (lowerQuartile, _, upperQuartile) => upperQuartile.minus(lowerQuartile)
        }

        def mean[A](xs: Vector[NumberLike[A]]): NumberLike[A] =
          xs.reduce(_.plus(_)).divide(xs.size)
      }
    }
  }

  //Type classes to the rescue!
  {
    printHeader("Type classes to the rescue!")

    //Creating a type class
    {
      object Math {

        trait NumberLike[T] {
          def plus(x: T, y: T): T
          def divide(x: T, y: Int): T
          def minus(x: T, y: T): T
        }
      }
    }

    //Providing default members
    {
      object Math {
        import annotation.implicitNotFound
        @implicitNotFound("No member of type class NumberLike in scope for ${T}")
        trait NumberLike[T] {
          def plus(x: T, y: T): T
          def divide(x: T, y: Int): T
          def minus(x: T, y: T): T
        }
        object NumberLike {
          implicit object NumberLikeDouble extends NumberLike[Double] {
            def plus(x: Double, y: Double): Double = x + y
            def divide(x: Double, y: Int): Double = x / y
            def minus(x: Double, y: Double): Double = x - y
          }
          implicit object NumberLikeInt extends NumberLike[Int] {
            def plus(x: Int, y: Int): Int = x + y
            def divide(x: Int, y: Int): Int = x / y
            def minus(x: Int, y: Int): Int = x - y
          }
        }
      }

      //Coding against type classes
      {
        printHeader("Coding against type classes")
        object Statistics {
          import Math.NumberLike
          def mean[T](xs: Vector[T])(implicit ev: NumberLike[T]): T =
            ev.divide(xs.reduce(ev.plus(_, _)), xs.size)

        }
        val numbers = Vector[Double](13, 23.0, 42, 45, 61, 73, 96, 100, 199, 420, 900, 3839)
        println(Statistics.mean(numbers))

        //compilation error => No member of type class NumberLike in scope for String
//        val snumbers = Vector[String]("13", "23.0", "42", "45", "61", "73", "96", "100", "199", "420", "900", "3839")
//        println(Statistics.mean(snumbers))
      }

      //Context bounds
      {
        printHeader("Context bounds")

        object Statistics {
          import Math.NumberLike
          def mean[T](xs: Vector[T])(implicit ev: NumberLike[T]): T =
            ev.divide(xs.reduce(ev.plus(_, _)), xs.size)

          //context bound
          def median[T : NumberLike](xs: Vector[T]): T = xs(xs.size / 2)

          def quartiles[T: NumberLike](xs: Vector[T]): (T, T, T) =
            (xs(xs.size / 4), median(xs), xs(xs.size / 4 * 3))

          def iqr[T: NumberLike](xs: Vector[T]): T = quartiles(xs) match {
            case (lowerQuartile, _, upperQuartile) =>
              implicitly[NumberLike[T]].minus(upperQuartile, lowerQuartile)
          }
        }

        //Custom type class members
        {
          printHeader("Custom type class members")
          object JodaImplicits {
            import Math.NumberLike
            import org.joda.time.Duration
            implicit object NumberLikeDuration extends NumberLike[Duration] {
              def plus(x: Duration, y: Duration): Duration = x.plus(y)
              def divide(x: Duration, y: Int): Duration = Duration.millis(x.getMillis / y)
              def minus(x: Duration, y: Duration): Duration = x.minus(y)
            }
          }

          import Statistics._
          import JodaImplicits._
          import org.joda.time.Duration._

          val durations = Vector(standardSeconds(20), standardSeconds(57), standardMinutes(2),
            standardMinutes(17), standardMinutes(30), standardMinutes(58), standardHours(2),
            standardHours(5), standardHours(8), standardHours(17), standardDays(1),
            standardDays(4))
          println(mean(durations).getStandardHours)
        }
      }
    }
  }
}
