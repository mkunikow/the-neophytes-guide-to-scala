package parts
import common.printer.printHeader

/**
 * Created by michal on 12/30/14.
 * The Neophyte's Guide to Scala Part 10: Staying DRY With Higher-order Functions
 */
object part10 extends App {

// On higher-order functions
// 1. One or more of its parameters is a function, and it returns some value.
// 2. It returns a function, but none of its parameters is a function.
// 3. Both of the above: One or more of its parameters is a function, and it returns a function.

//  And out of nowhere, a function was born
  {
    printHeader("And out of nowhere, a function was born")
    case class Email(
                      subject: String,
                      text: String,
                      sender: String,
                      recipient: String)

    type EmailFilter = Email => Boolean

    def newMailsForUser(mails: Seq[Email], f: EmailFilter) = mails.filter(f)

    val sentByOneOf: Set[String] => EmailFilter = senders => email => senders.contains(email.sender)
    val notSentByAnyOf: Set[String] => EmailFilter = senders => email => !senders.contains(email.sender)

    val minimumSize: Int => EmailFilter = n => email => email.text.size >= n
    val maximumSize: Int => EmailFilter = n => email => email.text.size <= n

    val emailFilter: EmailFilter = notSentByAnyOf(Set("johndoe@example.com"))

    val mails = Email(
      subject = "It's me again, your stalker friend!",
      text = "Hello my friend! How are you?",
      sender = "johndoe@example.com",
      recipient = "me@example.com") :: Nil

    val r = newMailsForUser(mails, emailFilter) // returns an empty list
    println(s"emails: $r")

    //Reusing existing functions
    {
      printHeader("Reusing existing functions")
      type SizeChecker = Int => Boolean
      val sizeConstraint: SizeChecker => EmailFilter = f => email => f(email.text.size)

      val minimumSize: Int => EmailFilter = n => sizeConstraint(_ >= n)
      val maximumSize: Int => EmailFilter = n => sizeConstraint(_ <= n)
    }

    //Function composition
    {
      printHeader("Function composition")

      def complement[A](predicate: A => Boolean) = (a: A) => !predicate(a)
//      val notSentByAnyOf = sentByOneOf andThen(g => complement(g))
      val notSentByAnyOf = sentByOneOf andThen(complement(_))

    //Composing predicates
      {
        printHeader("Composing predicates")
        def any[A](predicates: (A => Boolean)*): A => Boolean = a => predicates.exists(pred => pred(a))
        def none[A](predicates: (A => Boolean)*) = complement(any(predicates: _*))
        def every[A](predicates: (A => Boolean)*) = none(predicates.view.map(complement(_)): _*)

        val filter: EmailFilter = every(
          notSentByAnyOf(Set("johndoe@example.com")),
          minimumSize(100),
          maximumSize(10000)
        )
      }

      //Composing a transformation pipeline
      {
        printHeader("Composing a transformation pipeline")

        val addMissingSubject = (email: Email) =>
          if (email.subject.isEmpty) email.copy(subject = "No subject")
          else email
        val checkSpelling = (email: Email) =>
          email.copy(text = email.text.replaceAll("your", "you're"))
        val removeInappropriateLanguage = (email: Email) =>
          email.copy(text = email.text.replaceAll("dynamic typing", "**CENSORED**"))
        val addAdvertismentToFooter = (email: Email) =>
          email.copy(text = email.text + "\nThis mail sent via Super Awesome Free Mail")

        val pipeline = Function.chain(Seq(
          addMissingSubject,
          checkSpelling,
          removeInappropriateLanguage,
          addAdvertismentToFooter))
      }

    }

    //Higher-order functions and partial functions
    {
      printHeader("Higher-order functions and partial functions")

      //Chaining partial functions
      printHeader("Chaining partial functions")
//      val handler = fooHandler orElse barHandler orElse bazHandler

    }
  }
}
