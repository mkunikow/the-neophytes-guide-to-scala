package parts

import common.printer.printHeader

/**
 * Created by michal on 12/13/14.
 * The Neophyte's Guide to Scala Part 4: Pattern Matching Anonymous Functions
 */
object part4 extends App {
  {
    printHeader("Pattern Matching Anonymous Functions")

    val songTitles = List("The White Hare", "Childe the Hunter", "Take no Rogues")
    val songTitlesLowerCase = songTitles.map(_.toLowerCase)
    println(s"Song titles lowercase: $songTitlesLowerCase")



    val wordFrequencies = ("habitual", 6) :: ("and", 56) :: ("consuetudinary", 2) ::
      ("additionally", 27) :: ("homely", 5) :: ("society", 13) :: Nil

    def wordsWithoutOutliers(wordFrequencies: Seq[(String, Int)]): Seq[String] =
      wordFrequencies.filter(wf => wf._2 > 3 && wf._2 < 25).map(_._1)

    val wwo = wordsWithoutOutliers(wordFrequencies) // List("habitual", "homely", "society")
    println(s"wordsWithoutOutliers: $wwo")

    def wordsWithoutOutliersWithPatternMatching(wordFrequencies: Seq[(String, Int)]): Seq[String] =
      wordFrequencies.filter { case (_, f) => f > 3 && f < 25 } map { case (w, _) => w }

    val wwopm = wordsWithoutOutliersWithPatternMatching(wordFrequencies) // List("habitual", "homely", "society")
    println(s"wordsWithoutOutliersWithPatternMatching: $wwopm")

    val predicate: (String, Int) => Boolean = { case (_, f) => f > 3 && f < 25 }
    val transformFn: (String, Int) => String = { case (w, _) => w }
  }

  {
    printHeader("Partial functions")

    //def collect[B](pf: PartialFunction[A, B])

    val wordFrequencies = ("habitual", 6) :: ("and", 56) :: ("consuetudinary", 2) ::
      ("additionally", 27) :: ("homely", 5) :: ("society", 13) :: Nil

    val pf: PartialFunction[(String, Int), String] = {
      case (word, freq) if freq > 3 && freq < 25 => word
    }

    val pf2 = new PartialFunction[(String, Int), String] {
      def apply(wordFrequency: (String, Int)) = wordFrequency match {
        case (word, freq) if freq > 3 && freq < 25 => word
      }
      def isDefinedAt(wordFrequency: (String, Int)) = wordFrequency match {
        case (word, freq) if freq > 3 && freq < 25 => true
        case _ => false
      }
    }

    //pf == pf2
    assert(wordFrequencies.collect(pf) == wordFrequencies.collect(pf2))


//    wordFrequencies.map(pf) // will throw a MatchError

    val r = wordFrequencies.collect(pf) // List("habitual", "homely", "society")
    println(s"wordFrequencies.collect(pf): $r")


    def wordsWithoutOutliers(wordFrequencies: Seq[(String, Int)]): Seq[String] =
      wordFrequencies.collect { case (word, freq) if freq > 3 && freq < 25 => word }

    val r2 = wordsWithoutOutliers(wordFrequencies)
    println(s"wordsWithoutOutliers(wordFrequencies): $r2")
  }

}
