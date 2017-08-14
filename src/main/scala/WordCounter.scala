object WordCounter {
  def countWords(line: List[String]): Map[String, Int] = {
    val words = List("apple banana", "orange apple mango", "kiwi papaya orange", "mango orange muscat apple")

    var countWord = Map[String, Int]()

    for (phrase: String <- words) {
      for (word: String <- phrase.split(' ')) {
        if (countWord.isDefinedAt(word)) {
          countWord = countWord.updated(word, countWord(word) + 1)
        } else {
          countWord = countWord + (word -> 1)
        }
      }
    }

    words.map( phrase => phrase.split(' ') )


    countWord
  }
}