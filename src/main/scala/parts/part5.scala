package parts
import common.printer._

/**
 * Created by michal on 12/21/14.
 * The Neophyte's Guide to Scala Part 5: The Option Type
 */
object part5 extends App {

  case class User(
                   id: Int,
                   firstName: String,
                   lastName: String,
                   age: Int,
                   gender: Option[String])

  object UserRepository {
    private val users = Map(1 -> User(1, "John", "Doe", 32, Some("male")),
      2 -> User(2, "Johanna", "Doe", 30, None))
    def findById(id: Int): Option[User] = users.get(id)
    def findAll = users.values
  }


  {
    printHeader("Creating an option")

    val greetingSome: Option[String] = Some("Hello world")
    val greetingNone: Option[String] = None

    val absentGreeting: Option[String] = Option(null) // absentGreeting will be None
    val presentGreeting: Option[String] = Option("Hello!") // presentGreeting will be Some("Hello!")

    println(s"greetingSome: $greetingSome")
    println(s"greetingNone: $greetingNone")
    println(s"absentGreeting: $absentGreeting")
    println(s"presentGreeting: $presentGreeting")
  }

  {
    printHeader("Working with optional values")


    val user1 = UserRepository.findById(1)
    //bad access - stay away
    if (user1.isDefined) {
      println(user1.get.firstName)
    } // will print "John"


    printHeader("Providing a default value")

    val user = User(2, "Johanna", "Doe", 30, None)
    println("Gender: " + user.gender.getOrElse("not specified")) // will print "not specified"
  }

  {
    printHeader("Pattern matching")

    val user1 = User(2, "Johanna", "Doe", 30, None)
    user1.gender match {
      case Some(gender) => println("Gender: " + gender)
      case None => println("Gender: not specified")
    }

    val user2 = User(2, "Johanna", "Doe", 30, None)
    val gender = user2.gender match {
      case Some(gender) => gender
      case None => "not specified"
    }
    println("Gender: " + gender)

  }

  {
    printHeader("Options can be viewed as collections")

    {
      printHeader("Performing a side-effect if a value is present")
      val user4 = UserRepository.findById(2).foreach(user => println(user.firstName)) // prints "Johanna"
      println(user4)
    }
    {
      printHeader("Mapping an option")
      val age = UserRepository.findById(1).map(_.age) // age is Some(32)
      println(s"age: $age")
    }
    {

      printHeader("flatMap and options")
      val gender = UserRepository.findById(1).map(_.gender) // gender is an Option[Option[String]]
      println(s"gender: $gender")

      val gender1 = UserRepository.findById(1).flatMap(_.gender) // gender is Some("male")
      val gender2 = UserRepository.findById(2).flatMap(_.gender) // gender is None
      val gender3 = UserRepository.findById(3).flatMap(_.gender) // gender is None
      println(s"gender1: $gender1")
      println(s"gender2: $gender2")
      println(s"gender3: $gender3")

      val names: List[List[String]] = List(List("John", "Johanna", "Daniel"), List(), List("Doe", "Westheide"))
      val namesUpperCaseMap = names.map(_.map(_.toUpperCase))
      // results in List(List("JOHN", "JOHANNA", "DANIEL"), List(), List("DOE", "WESTHEIDE"))
      val namesUpperCaseFlatMap = names.flatMap(_.map(_.toUpperCase))
      // results in List("JOHN", "JOHANNA", "DANIEL", "DOE", "WESTHEIDE")

      println(s"namesUpperCaseMap: $namesUpperCaseMap")
      println(s"namesUpperCaseFlaMap: $namesUpperCaseFlatMap")

      val namesOpt: List[Option[String]] = List(Some("Johanna"), None, Some("Daniel"))
      val namesOptUpperCaseMap = namesOpt.map(_.map(_.toUpperCase)) // List(Some("JOHANNA"), None, Some("DANIEL"))
      val namesOptUpperCaseFlatMap = namesOpt.flatMap(xs => xs.map(_.toUpperCase)) // List("JOHANNA", "DANIEL")

      println(s"namesOptUpperCaseMap: $namesOptUpperCaseMap")
      println(s"namesOptUpperCaseFlaMap: $namesOptUpperCaseFlatMap")
    }

  }

  {
    printHeader("Filtering an option")

    val filter1 = UserRepository.findById(1).filter(_.age > 30) // None, because age is <= 30
    val filter2 = UserRepository.findById(2).filter(_.age > 30) // Some(user), because age is > 30
    val filter3 = UserRepository.findById(3).filter(_.age > 30) // None, because user is already None

    println(filter1)
    println(filter2)
    println(filter3)

  }

  {
    printHeader("For comprehensions")

    val genders = for {
      user <- UserRepository.findById(1)
      gender <- user.gender
    } yield gender // results in Some("male")

    println(genders)

    val allGenders = for {
      user <- UserRepository.findAll
      gender <- user.gender
    } yield gender

    println(allGenders)

  }
  {
    printHeader("Usage in the left side of a generator")

    val genders = for {
      User(_, _, _, _, Some(gender)) <- UserRepository.findAll
    } yield gender
    println(genders)
  }
  {
    printHeader("Chaining options")

    case class Resource(content: String)
    val resourceFromConfigDir: Option[Resource] = None
    val resourceFromClasspath: Option[Resource] = Some(Resource("I was found on the classpath"))
    val resource = resourceFromConfigDir orElse resourceFromClasspath

    println(resource)
  }
}
