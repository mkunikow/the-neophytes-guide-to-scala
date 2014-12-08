package parts.part1
import common.printer.printHeader

/**
 * Created by michal on 12/7/14.
 * The Neophyte's Guide to Scala Part 1: Extractors
 */
object Part1 extends App {
    {
        printHeader("Extracting list values")

        case class User(firstName: String, lastName: String, score: Int)

        def advance(xs: List[User]) = xs match {
            case User(_, _, score1) :: User(_, _, score2) :: _ => score1 - score2
            case _ => 0
        }

        val harry = new User("Harry", "Potter", 30)
        val rubeus = new User("Rubeus", "Hagrid", 10)

        println("Harry Rubeus score diff: " + advance(List(harry, rubeus)))
    }


    //Extracting single value
    //def unapply(object: S): Some[(T1)]
    {
        printHeader("Extracting single value")


        trait User {
            def name: String
        }
        class FreeUser(val name: String) extends User
        class PremiumUser(val name: String) extends User

        object FreeUser {
            def unapply(user: FreeUser): Option[String] = Some(user.name)
        }
        object PremiumUser {
            def unapply(user: PremiumUser): Option[String] = Some(user.name)
        }

        println(FreeUser.unapply(new FreeUser("Daniel")))



        val user: User = new PremiumUser("Daniel")

        val result = user match {
            case FreeUser(name) => "Hello " + name
            case PremiumUser(name) => "Welcome back, dear " + name
        }
        println(result)

    }

    //Extracting several values
    //def unapply(object: S): Option[(T1, ..., Tn)]
    {
        printHeader("Extracting several values")

        trait User {
            def name: String
            def score: Int
        }

        class FreeUser(val name: String, val score: Int, val upgradeProbability: Double)
            extends User

        class PremiumUser(val name: String, val score: Int) extends User

        object FreeUser {
            def unapply(user: FreeUser): Option[(String, Int, Double)] =
                Some((user.name, user.score, user.upgradeProbability))
        }

        object PremiumUser {
            def unapply(user: PremiumUser): Option[(String, Int)] = Some((user.name, user.score))
        }

        val user: User = new FreeUser("Daniel", 3000, 0.7d)

        val r = user match {
            case FreeUser(name, _, p) =>
                if (p > 0.75) name + ", what can we do for you today?" else "Hello " + name
            case PremiumUser(name, _) => "Welcome back, dear " + name
        }

        println("" + r)
    }

    //A Boolean extractor
    //def unapply(object: S): Boolean
    {
        printHeader("A Boolean extractor")

        trait User {
            def name: String
            def score: Int
            override def toString = s"User(name: $name, score: $score)"

        }

        class FreeUser(val name: String, val score: Int, val upgradeProbability: Double)
            extends User

        class PremiumUser(val name: String, val score: Int) extends User

        object FreeUser {
            def unapply(user: FreeUser): Option[(String, Int, Double)] =
                Some((user.name, user.score, user.upgradeProbability))
        }

        object PremiumUser {
            def unapply(user: PremiumUser): Option[(String, Int)] = Some((user.name, user.score))
        }

        object premiumCandidate {
            def unapply(user: FreeUser): Boolean = user.upgradeProbability > 0.75
        }

        def initiateSpamProgram(user: User) = println(s"initial spam program to user: $user")
        def sendRegularNewsletter(user: User) = println(s"initial spam program to user: $user")

        val user: User = new FreeUser("Daniel", 2500, 0.8d)

        user match {
            case freeUser @ premiumCandidate() => initiateSpamProgram(freeUser)
            case _ => sendRegularNewsletter(user)
        }
    }

    //Infix operation patterns
    {
        printHeader("Infix operation patterns")

        val xs = 58 #:: 43 #:: 93 #:: Stream.empty
        val r = xs match {
            case first #:: second #:: _ => first - second
            case _ => -1
        }
        println(r)
    }

//    A closer look at the Stream extractor
//    Here is the complete extractor, taken from the sources of Scala 2.9.2:

//    object #:: {
//        def unapply[A](xs: Stream[A]): Option[(A, Stream[A])] =
//            if (xs.isEmpty) None
//            else Some((xs.head, xs.tail))
//    }

    {
        printHeader("A closer look at the Stream extractor")

        val xs = 58 #:: 43 #:: 93 #:: Stream.empty
        val r = xs match {
            case #::(first, #::(second, _)) => first - second
            case _ => -1
        }
        println(r)
    }

}
