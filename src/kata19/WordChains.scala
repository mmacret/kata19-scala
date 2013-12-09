package kata19

import common._

object WordChains extends App {

  /** A word is simply a `String`. */
  type Word = String

  /**
   * A Path is a sequence of words.
   */
  class Path(history: List[Word]) {
    def endWord: Word = history.head
    def extend(word: Word) = new Path(word :: history)
    override def toString = (history reverse) mkString " "
  }

  /**
   * The dictionary is simply a sequence of words.
   *  It is predefined and obtained as a sequence using the utility method `loadDictionary`.
   */
  val dictionary: List[Word] = loadDictionary

  /**
   * Returns how many letters differs between word1 and word2.
   */
  def wordDistance(word1: Word, word2: Word): Int = {
    word1.zip(word2).filter(x => (x._1 != x._2)).size
  }

  /**
   * Returns a list of words that differ only by one letter from the word 'word'.
   */
  def word1ldiffer(word: Word): List[Word] = dictionary.filter(x => (x.size == word.size) && (wordDistance(word, x) == 1))

  /**
   * The function `from` returns the stream of all possible paths
   * that can be followed, starting at the `endWord` of the `initial`
   * path.
   *
   * The parameter `explored` is a set of words that have
   * been visited before, on the path to any of the word in the
   * Path `initial`. When search reaches a word that has already
   * been explored before, that word won't be used a second time
   * to avoid circles.
   *
   * The resulting stream is sorted by ascending path length,
   * i.e. the word that can be reached with the fewest
   * amount of steps appears first in the stream.
   */
  def from(initial: Set[Path], explored: Set[Word]): Stream[Set[Path]] =
    if (initial.isEmpty) Stream.empty
    else {
      val more = for {
        path <- initial
        next <- word1ldiffer(path.endWord) map path.extend
      } yield next
      initial #:: from(more, explored ++ (more map (_.endWord)))
    }

  /**
   * The (or one of the) shortest sequence(s) of steps to reach the
   * target word.
   */
  def findChain(start: Word, target: Word): Stream[Path] = {
    if (!dictionary.contains(start) || !dictionary.contains(target)) throw new Exception("Words not in the dictionary")
    if (start.size != target.size) throw new Exception("Words should have the same length")

    val initialPath = new Path(List(start))
    lazy val pathSets = from(Set(initialPath), Set(start))

    for {
      pathSet <- pathSets
      path <- pathSet
      if path.endWord contains target

    } yield path
  }
  
  lazy val cat2dog = findChain("cat","dog")
  lazy val lead2gold = findChain("lead","gold")
  lazy val ruby2code = findChain("ruby","code")
 
 println("Go from cat to dog:")
 cat2dog take 1 print
 
 println("\n")
 
 println("Go from lead to gold:")
 lead2gold take 1 print
 
 println("\n")
  
 println("Go from ruby to code:")
 ruby2code take 1 print
 
 
 
}