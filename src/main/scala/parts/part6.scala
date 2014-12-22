package parts
import common.printer._

/**
 * Created by michal on 12/22/14.
 * Part 6: Error Handling With Try
 */
object part6 extends App {


  //Throwing and catching exceptions
  {
    printHeader("Throwing and catching exceptions")

    case class Customer(age: Int)
    class Cigarettes
    case class UnderAgeException(message: String) extends Exception(message)

    def buyCigarettes(customer: Customer): Cigarettes =
      if (customer.age < 16)
        throw UnderAgeException(s"Customer must be older than 16 but was ${customer.age}")
      else new Cigarettes

    val youngCustomer = Customer(15)
    try {
      buyCigarettes(youngCustomer)
      "Yo, here are your cancer sticks! Happy smoking'!"
    } catch {
      case UnderAgeException(msg) => msg
    }

  }

  //Error handling, the functional way

  {
    //The semantics of Try
    printHeader("The semantics of Try")
    import scala.util.Try
    import java.net.URL
    def parseURL(url: String): Try[URL] = Try(new URL(url))

    {
      //Working with Try values
      printHeader("Working with Try values")
      val url = parseURL("http://danielwestheide.com") getOrElse new URL("http://duckduckgo.com")
      println(url)
    }

    {
      //Chaining operations
      printHeader("Chaining operations")

      {
        //Mapping and flat mapping
        printHeader("Mapping and flat mapping")

        val r1 = parseURL("http://danielwestheide.com").map(_.getProtocol)
        // results in Success("http")
        println(r1)

        val r2 = parseURL("garbage").map(_.getProtocol)
        // results in Failure(java.net.MalformedURLException: no protocol: garbage)
        println(r2)

        import java.io.InputStream
        def inputStreamForURL(url: String): Try[Try[Try[InputStream]]] = parseURL(url).map { u =>
          Try(u.openConnection()).map(conn => Try(conn.getInputStream))
        }

        //using flat map to get Try[InputStream]
        def inputStreamForURLWithFlatMap(url: String): Try[InputStream] = parseURL(url).flatMap { u =>
          Try(u.openConnection()).flatMap(conn => Try(conn.getInputStream))
        }
      }

      {
        //Filter and foreach
        printHeader("Filter and foreach")

        def parseHttpURL(url: String) = parseURL(url).filter(_.getProtocol == "http")
        val r1 = parseHttpURL("http://apache.openmirror.de") // results in a Success[URL]
        val r2 = parseHttpURL("ftp://mirror.netcologne.de/apache.org") // results in a Failure[URL]
        println(r1)
        println(r2)

        parseHttpURL("http://danielwestheide.com").foreach(println)

      }

      {
        //For comprehensions
        printHeader("For comprehensions")

        import scala.io.Source
        def getURLContent(url: String): Try[Iterator[String]] =
          for {
            url <- parseURL(url)
            connection <- Try(url.openConnection())
            is <- Try(connection.getInputStream)
            source = Source.fromInputStream(is)
          } yield source.getLines()

        val r = getURLContent("http://danielwestheide.com")
        println(r)

        {
          //Pattern Matching
          printHeader("Pattern Matching")

          import scala.util.Success
          import scala.util.Failure
          getURLContent("http://danielwestheide.com/foobar") match {
            case Success(lines) => lines.foreach(println)
            case Failure(ex) => println(s"Problem rendering URL content: ${ex.getMessage}")
          }
        }

        {
          //Recovering from a Failure
          printHeader("Recovering from a Failure")

          import java.net.MalformedURLException
          import java.io.FileNotFoundException
          val content = getURLContent("garbage") recover {
            case e: FileNotFoundException => Iterator("Requested page does not exist")
            case e: MalformedURLException => Iterator("Please make sure to enter a valid URL")
            case _ => Iterator("An unexpected error has occurred. We are so sorry!")
          }
          println(content)
        }
      }
    }
  }
}
