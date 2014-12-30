package parts
import common.printer.printHeader

/**
 * Created by michal on 12/30/14.
 * The Neophyte's Guide to Scala Part 11: Currying and Partially Applied Functions
 */
object part11 extends App {

  //Partially applied functions
  {
   printHeader("Partially applied functions")

    case class Email(
                      subject: String,
                      text: String,
                      sender: String,
                      recipient: String)

    type EmailFilter = Email => Boolean

    type IntPairPred = (Int, Int) => Boolean

    def sizeConstraint(pred: IntPairPred, n: Int, email: Email) = pred(email.text.size, n)

    val gt: IntPairPred = _ > _
    val ge: IntPairPred = _ >= _
    val lt: IntPairPred = _ < _
    val le: IntPairPred = _ <= _
    val eq: IntPairPred = _ == _

    val minimumSize: (Int, Email) => Boolean = sizeConstraint(ge, _: Int, _: Email)
    val maximumSize: (Int, Email) => Boolean = sizeConstraint(le, _: Int, _: Email)

    val constr20: (IntPairPred, Email) => Boolean = sizeConstraint(_: IntPairPred, 20, _: Email)
    val constr30: (IntPairPred, Email) => Boolean = sizeConstraint(_: IntPairPred, 30, _: Email)

    //From methods to function objects
    {
      printHeader("From methods to function objects")
      val sizeConstraintFn: (IntPairPred, Int, Email) => Boolean = sizeConstraint _
    }

    //Producing those EmailFilters
    {
      printHeader("Producing those EmailFilters")

      {
        val min20: EmailFilter = minimumSize(20, _: Email)
        val max20: EmailFilter = maximumSize(20, _: Email)
      }
      {

        val min20: EmailFilter = constr20(ge, _: Email)
        val max20: EmailFilter = constr20(le, _: Email)
      }

    }

    //Spicing up your functions
    {
      printHeader("Spicing up your functions")
      def sizeConstraint(pred: IntPairPred)(n: Int)(email: Email): Boolean = pred(email.text.size, n)
      val sizeConstraintFn: IntPairPred => Int => Email => Boolean = sizeConstraint _

      val minSize: Int => Email => Boolean = sizeConstraint(ge)
      val maxSize: Int => Email => Boolean = sizeConstraint(le)

      {
        val min20: Email => Boolean = minSize(20)
        val max20: Email => Boolean = maxSize(20)
      }

      {
        val min20: Email => Boolean = sizeConstraintFn(ge)(20)
        val max20: Email => Boolean = sizeConstraintFn(le)(20)
      }

    }

    //Currying existing functions
    {
      printHeader("Currying existing functions")

      val sum: (Int, Int) => Int = _ + _
      val sumCurried: Int => Int => Int = sum.curried
    }

    //Injecting your dependencies the functional way
    {
      printHeader("Injecting your dependencies the functional way")

      case class User(name: String)

      trait EmailRepository {
        def getMails(user: User, unread: Boolean): Seq[Email]
      }

      trait FilterRepository {
        def getEmailFilter(user: User): EmailFilter
      }

      trait MailboxService {
        def getNewMails(emailRepo: EmailRepository)(filterRepo: FilterRepository)(user: User) =
          emailRepo.getMails(user, true).filter(filterRepo.getEmailFilter(user))
        val newMails: User => Seq[Email]
      }


      object MockEmailRepository extends EmailRepository {
        def getMails(user: User, unread: Boolean): Seq[Email] = Nil
      }

      object MockFilterRepository extends FilterRepository {
        def getEmailFilter(user: User): EmailFilter = _ => true
      }

      object MailboxServiceWithMockDeps extends MailboxService {
        val newMails: (User) => Seq[Email] =
          getNewMails(MockEmailRepository)(MockFilterRepository) _
      }

      MailboxServiceWithMockDeps.newMails(User("daniel"))

    }
  }

}
