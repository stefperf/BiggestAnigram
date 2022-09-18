/*
solving Riddler Classic @ https://fivethirtyeight.com/features/can-you-build-the-biggest-anigram/
*/
import scala.collection.SortedMap
import scala.collection.mutable.{SortedMap => MutableSortedMap, SortedSet => MutableSortedSet}
import scala.io.Source

case class Anagram(letterFreqs: SortedMap[Char, Int]) extends Ordered[Anagram] {
  val wordLength = letterFreqs.values.sum
  val letterSeq = letterFreqs.map{ case (ch, n) => ch.toString * n }.mkString

  override def toString: String = letterSeq

  def potentialMinors: Iterable[Anagram] = letterFreqs.map { case (ch, n) =>
    if (n == 1) new Anagram(SortedMap.empty[Char, Int] ++ (letterFreqs - ch))
    else new Anagram(SortedMap.empty[Char, Int] ++ (letterFreqs ++ Map(ch -> (n - 1))))
  }

  def firstMinor(anagrams: Set[Anagram]): Option[Anagram] = potentialMinors.find(anagrams.contains(_))

  def minors(anagrams: Set[Anagram]): Iterable[Anagram] = potentialMinors.filter(anagrams.contains(_))

  override def compare(that: Anagram): Int = letterSeq compare that.letterSeq
}

object Anagram {
  def apply(word: String): Anagram = Anagram(
    SortedMap.empty[Char, Int] ++ word.toSeq.groupBy { ch: Char => ch }.map{ t => t._1 -> t._2.length }
  )
}


object BiggestAnigram extends App {
  // source: https://norvig.com/ngrams/enable1.txt
  val wordsFile = "https_colon_slash_slash_norvig_dot_com_slash_ngrams_slash_enable1.txt"
  val minWordLength = 4
  val wordsByLength = SortedMap.empty[Int, Seq[String]] ++
    Source.fromFile(wordsFile).getLines.filter(_.length >= minWordLength).toSeq.groupBy(_.length)
  val anagramClassesByLength = wordsByLength.map(_._1 -> MutableSortedMap.empty[Anagram, MutableSortedSet[String]])
  (wordsByLength zip anagramClassesByLength).foreach { case ((wordLength, words), (_, anagramClasses)) =>
    words.foreach { word =>
      val anagram = Anagram(word)
      val previousAnagrams = anagramClassesByLength.get(wordLength - 1).fold(Set.empty[Anagram])(_.keys.toSet)
      if (wordLength == minWordLength || anagram.firstMinor(previousAnagrams).isDefined) {
        if (anagramClasses.contains(anagram)) anagramClasses(anagram).add(word)
        else anagramClasses(anagram) = MutableSortedSet(word)
      }
    }
    println(s"word length = $wordLength: ${words.size} words, ${anagramClasses.map(_._2.size).sum} linkable words in ${anagramClasses.size} anagram classes")
  }
  val chains = anagramClassesByLength.filter(_._2.nonEmpty)

  var wordChain = List.empty[String]
  var wordLength = chains.last._1
  var anagram = chains(wordLength).keys.head
  var word = chains(wordLength)(anagram).head
  while (wordLength > minWordLength) {
    wordChain = word :: wordChain
    wordLength -= 1
    anagram = anagram.firstMinor(chains(wordLength).keys.toSet).get
    word = chains(wordLength)(anagram).head
  }
  wordChain = word :: wordChain
  println(s"One of the biggest anigrams is ${wordChain.mkString(" >> ")}")
}
