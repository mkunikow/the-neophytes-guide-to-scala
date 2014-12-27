package parts
import common.printer._

/**
 * Created by michal on 12/26/14.
 * The Neophyte's Guide to Scala Part 7: The Either Type
 */
object part7 extends App {

  //Creating an Either
  {
    printHeader("Creating an Either")
    import scala.io.Source
    import java.net.URL
    def getContent(url: URL): Either[String, Source] =
      if (url.getHost.contains("google"))
        Left("Requested URL is blocked for the good of the people!")
      else
        Right(Source.fromURL(url))

    {
      printHeader("Working with Either values")
      getContent(new URL("http://google.com")) match {
        case Left(msg) => println(msg)
        case Right(source) => source.getLines.foreach(println)
      }
    }

    //Projections
    {
      printHeader("Projections")
//      You cannot, at least not directly, use an Either instance like a collection,
//      the way you are familiar with from Option and Try.
//      This is because Either is designed to be unbiased.

      {
        printHeader("Mapping")
        val content: Either[String, Iterator[String]] =
        getContent(new URL("http://danielwestheide.com")).right.map(_.getLines())
        // content is a Right containing the lines from the Source returned by getContent
        val moreContent: Either[String, Iterator[String]] =
          getContent(new URL("http://google.com")).right.map(_.getLines)
        // moreContent is a Left, as already returned by getContent
      }

      {
        val content: Either[Iterator[String], Source] =
          getContent(new URL("http://danielwestheide.com")).left.map(Iterator(_))
        // content is the Right containing a Source, as already returned by getContent
        val moreContent: Either[Iterator[String], Source] =
          getContent(new URL("http://google.com")).left.map(Iterator(_))
        // moreContent is a Left containing the msg returned by getContent in an Iterator
      }

      //Flat mapping
      {
        printHeader("Flat mapping")
        val part5 = new URL("http://t.co/UR1aalX4")
        val part6 = new URL("http://t.co/6wlKwTmu")
        val content: Either[String, Either[String, Int]] = getContent(part5).right.map(a =>
          getContent(part6).right.map(b =>
            (a.getLines().size + b.getLines().size) / 2))

        {
          val content: Either[String, Int] = getContent(part5).right.flatMap(a =>
            getContent(part6).right.map(b =>
              (a.getLines().size + b.getLines().size) / 2))
        }
      }

      //For comprehensions
      {
        printHeader("For comprehensions")
        def averageLineCount(url1: URL, url2: URL): Either[String, Int] =
          for {
            source1 <- getContent(url1).right
            source2 <- getContent(url2).right
          } yield (source1.getLines().size + source2.getLines().size) / 2

          {
            //error => won't comile
//            def averageLineCountWontCompile(url1: URL, url2: URL): Either[String, Int] =
//              for {
//                source1 <- getContent(url1).right
//                source2 <- getContent(url2).right
//                lines1 = source1.getLines().size
//                lines2 = source2.getLines().size
//              } yield (lines1 + lines2) / 2

//            the same as

//            def averageLineCountDesugaredWontCompile(url1: URL, url2: URL): Either[String, Int] =
//              getContent(url1).right.flatMap { source1 =>
//                getContent(url2).right.map { source2 =>
//                  val lines1 = source1.getLines().size
//                  val lines2 = source2.getLines().size
//                  (lines1, lines2)
//                }.map { case (x, y) => x + y / 2 }
//              }
//            The problem is that by including a value definition in our for comprehension, a new call to map is
//            introduced automatically – on the result of the previous call to map, which has returned an Either,
//            not a RightProjection. As you know, Either doesn’t define a map method, making the compiler a little bit grumpy.

            //fix
            {
              def averageLineCount(url1: URL, url2: URL): Either[String, Int] =
              for {
                source1 <- getContent(url1).right
                source2 <- getContent(url2).right
                lines1 <- Right(source1.getLines().size).right
                lines2 <- Right(source2.getLines().size).right
              } yield (lines1 + lines2) / 2

            }

          }
      }

    }

    //Folding
    {
      printHeader("Folding")
      val content: Iterator[String] =
        getContent(new URL("http://danielwestheide.com")).fold(Iterator(_), _.getLines())
      val moreContent: Iterator[String] =
        getContent(new URL("http://google.com")).fold(Iterator(_), _.getLines())
    }

  }

  //When to use Either

  //Error handling
  {
    printHeader("Error handling")

    import scala.util.control.Exception.catching
    import java.net.URL

    //catch only certain type of exception
    def handling[Ex <: Throwable, T](exType: Class[Ex])(block: => T): Either[Ex, T] =
      catching(exType).either(block).asInstanceOf[Either[Ex, T]]

    import java.net.MalformedURLException
    def parseURL(url: String): Either[MalformedURLException, URL] =
      handling(classOf[MalformedURLException])(new URL(url))


    //better do it with Try
    case class Customer(age: Int)
    class Cigarettes
    case class UnderAgeFailure(age: Int, required: Int)
    def buyCigarettes(customer: Customer): Either[UnderAgeFailure, Cigarettes] =
      if (customer.age < 16) Left(UnderAgeFailure(customer.age, 16))
      else Right(new Cigarettes)

//  You should avoid using Either for wrapping unexpected exceptions. Try does that better,
//  without all the flaws you have to deal with when working with Either.

  }

  //Processing collections
  {
    printHeader("Processing collections")

    import java.net.URL
    type Citizen = String
    case class BlackListedResource(url: URL, visitors: Set[Citizen])

    val blacklist = List(
      BlackListedResource(new URL("https://google.com"), Set("John Doe", "Johanna Doe")),
      BlackListedResource(new URL("http://yahoo.com"), Set.empty),
      BlackListedResource(new URL("https://maps.google.com"), Set("John Doe")),
      BlackListedResource(new URL("http://plus.google.com"), Set.empty)
    )

    println(s"blacklist: $blacklist")

    val checkedBlacklist: List[Either[URL, Set[Citizen]]] =
      blacklist.map(resource =>
        if (resource.visitors.isEmpty) Left(resource.url)
        else Right(resource.visitors))

    println(s"checkedBlacklist $checkedBlacklist")

    val suspiciousResources = checkedBlacklist.flatMap(_.left.toOption)
    val problemCitizens = checkedBlacklist.flatMap(_.right.toOption).flatten.toSet

    println(s"suspiciousResources $suspiciousResources")
    println(s"problemCitizens $problemCitizens")
  }
}
