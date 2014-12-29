package parts
import common.printer.printHeader

import scala.concurrent.Future

/**
 * Created by michal on 12/29/14.
 * The Neophyte's Guide to Scala Part 9: Promises and Futures in Practice
 */
object part9 extends App {

  //Promises
  {
    printHeader("Promises")
    import concurrent.Future
    import concurrent.ExecutionContext.Implicits.global
    val f: Future[String] = Future { "Hello world!" }
    // REPL output:
    // f: scala.concurrent.Future[String] = scala.concurrent.impl.Promise$DefaultPromise@793e6657

      //Promising a rosy future
    {
      printHeader("Promising a rosy future")
      import concurrent.Promise
      case class TaxCut(reduction: Int)
      // either give the type as a type parameter to the factory method:
      val taxcut = Promise[TaxCut]()
      // or give the compiler a hint by specifying the type of your val:
      val taxcut2: Promise[TaxCut] = Promise()

      val taxcutF: Future[TaxCut] = taxcut.future

      //Completing a Promise
      {
        printHeader("Completing a Promise")

        //Delivering on your Promise
        {
          printHeader("Delivering on your Promise")
          taxcut.success(TaxCut(20))

          object Government {
            def redeemCampaignPledge(): Future[TaxCut] = {
              val p = Promise[TaxCut]()
              Future {
                println("Starting the new legislative period.")
                Thread.sleep(2000)
                p.success(TaxCut(20))
                println("We reduced the taxes! You must reelect us!!!!1111")
              }
              p.future
            }
          }

          import scala.util.{Success, Failure}
          val taxCutF: Future[TaxCut] = Government.redeemCampaignPledge()
          println("Now that they're elected, let's see if they remember their promises...")
          taxCutF.onComplete {
            case Success(TaxCut(reduction)) =>
              println(s"A miracle! They really cut our taxes by $reduction percentage points!")
            case Failure(ex) =>
              println(s"They broke their promises! Again! Because of a ${ex.getMessage}")
          }

        }
        //Breaking Promises like a sir
        {
          printHeader("Breaking Promises like a sir")

          case class LameExcuse(msg: String) extends Exception(msg)
          object Government {
            def redeemCampaignPledge(): Future[TaxCut] = {
              val p = Promise[TaxCut]()
              Future {
                println("Starting the new legislative period.")
                Thread.sleep(2000)
                p.failure(LameExcuse("global economy crisis"))
                println("We didn't fulfill our promises, but surely they'll understand.")
              }
              p.future
            }
          }
        }

      }
    }

  }

  //Future-based programming in practice
  {
    printHeader("Future-based programming in practice")

    //Blocking IO
    {
      printHeader("Blocking IO")
      // get back a Future[ResultSet] or something similar:
//      Future {
//        queryDB(query) //blocking code place in feature
//      }

//      So far, we always used the implicitly available global ExecutionContext to execute such future blocks.
//      It’s probably a good idea to create a dedicated ExecutionContext that you will have in scope in your database layer.
//
//      You can create an ExecutionContext from a Java ExecutorService,
//      which means you will be able to tune the thread pool for executing your database calls asynchronously
//      independently from the rest of your application:

      import java.util.concurrent.Executors
      import concurrent.ExecutionContext
      val executorService = Executors.newFixedThreadPool(4)
      val executionContext = ExecutionContext.fromExecutorService(executorService)

    }

    //Long-running computations
    {
      printHeader("Long-running computations")

//      Depending on the nature of your application, it will occasionally have to call long-running tasks that don’t
//      involve any IO at all, which means they are CPU-bound. These, too, should not be executed by a web server thread.
//      Hence, you should turn them into Futures, too:

//      Future {
//        longRunningComputation(data, moreData)
//      }

//      Again, if you have long-running computations, having them run in a separate ExecutionContext for CPU-bound
//      tasks is a good idea. How to tune your various thread pools is highly dependent on your individual application
//      and beyond the scope of this article.
    }
  }


}
