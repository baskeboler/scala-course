import forcomp.Anagrams._
object anagramsws {
  val sentence = List("Victor", "Guerrero")
  val sentence2 = List("Lin", "Zulu", "Rex")
  val ocs = sentenceOccurrences(sentence)
  val ocs2 = sentenceOccurrences(sentence2)
  val combos = combinations(ocs)
  val combos2 = combinations(ocs2)
  sentenceOccurrences(List())
  combinations(List())
  combinations(List(('a', 2), ('b', 2)))
 val sAnagrams=   sentenceAnagrams(sentence)
  val sAnagrams2 = sentenceAnagrams(sentence2)
  subtract(ocs, ocs)
  ocs == ocs2
}