import forcomp.Anagrams._
object Anagramsws {
  def show(l: List[Sentence]): String =
    l.map(_.mkString(" ")).mkString("\n")
  val sentence = List("Maria", "Guerrero")
  val s = "maria del rosario guerrero".split(" ").toList
  show(sentenceAnagrams(s).sortBy(_.size).take(100).toList)
}