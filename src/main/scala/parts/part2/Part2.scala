package parts.part2
import common.printer.printHeader
/**
 * Created by michal on 12/7/14.
 * The Neophyte's Guide to Scala Part 2: Extracting Sequences
 */
object Part2 extends App {
    {
        printHeader("Pattern matches some number of elements")
        val xs = 3 :: 6 :: 12 :: Nil
        val r = xs match {
            case List(a, b) => a * b
            case List(a, b, c) => a + b + c
            case _ => 0
        }

        println(r)
    }


    {
        printHeader("Pattern matches lists the exact length of which you don’t care")
        val xs = 3 :: 6 :: 12 :: 24 :: Nil
        val r = xs match {
            case List(a, b, _*) => a * b
            case _ => 0
        }

        println(r)
    }

    //def unapplySeq(object: S): Option[Seq[T]]

    {
        printHeader("Example: Extracting given names")

        object GivenNames {
            def unapplySeq(name: String): Option[Seq[String]] = {
                val names = name.trim.split(" ")
                if (names.forall(_.isEmpty)) None else Some(names)
            }
        }

        def greetWithFirstName(name: String) = name match {
            case GivenNames(firstName, _*) => "Good morning, " + firstName + "!"
            case _ => "Welcome! Please make sure to fill in your name!"
        }

        println(greetWithFirstName(""))
        println(greetWithFirstName("Bob"))
        println(greetWithFirstName("Bob Uncle"))
        println(greetWithFirstName("Bob Uncle Sir"))
    }

    //def unapplySeq(object: S): Option[(T1, .., Tn-1, Seq[T])]

    {
        printHeader("Combining fixed and variable parameter extraction")

        object Names {
            def unapplySeq(name: String): Option[(String, String, Seq[String])] = {
                val names = name.trim.split(" ")
                if (names.size < 2) None
                else Some((names.last, names.head, names.drop(1).dropRight(1).toSeq))
            }
        }

        def greet(fullName: String) = fullName match {
            case Names(lastName, firstName, _*) => "Good morning, " + firstName + " " + lastName + "!"
            case _ => "Welcome! Please make sure to fill in your name!"
        }

        println(greet(""))
        println(greet("Bob"))
        println(greet("Bob LastName"))
        println(greet("Bob Uncle LastName"))
        println(greet("Bob Uncle Sir LastName"))
    }

}
