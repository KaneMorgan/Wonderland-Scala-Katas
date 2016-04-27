object AlphabetCipher {

  val alphabet = ('a' to 'z').mkString("")

  def encode(keyword: String, message: String): String = {
    val ciphers = repeatKeyWord(keyword,message)
    message.zip(ciphers).map{case (x:Char,y:Char) => encodeCharacter(x,y)}.mkString("")
  }
  
  def encodeCharacter(cipher:Char,raw:Char):String =
  encodeCharacter(cipher.toString,raw.toString)

  def encodeCharacter(cipher:String, raw:String):String =
  offsetAlphabet(cipher).charAt(alphabet.indexOf(raw)).toString()

  def decode(keyword: String, message: String): String = {
    val ciphers = repeatKeyWord(keyword,message)
    message.zip(ciphers).map{case (x:Char,y:Char) => decodeCharacter(x.toString, y.toString)}.mkString("")
  }

  def decodeCharacter(cipher:String, raw:String):String ={
    val offset = offsetAlphabet(raw)
    alphabet.charAt(offset.indexOf(cipher)).toString
  }
  def decipher(cipher: String, message: String): String = {
    val repeatedCipher = cipher.zip(message).map{case (encoded, original) =>toCipher(encoded, original)}.mkString("")
    longestRepetition(repeatedCipher, 2)
  }

  def longestRepetition(repeatedCipher: String, subStringLength:Int):String = {
    val oldSub = repeatedCipher.substring(0,subStringLength-1)
    val previousIndex = repeatedCipher.lastIndexOf(oldSub)
    val subString = repeatedCipher.substring(0,subStringLength)
    val lastIndex = repeatedCipher.lastIndexOf(subString)

    if(previousIndex > lastIndex )
      repeatedCipher.substring(lastIndex,previousIndex)
    else
      longestRepetition(repeatedCipher, subStringLength + 1)

  }

  def longestSequence(chars:String,fullString:String):String = {
    if(fullString.lastIndexOf(chars) == 0)
      fullString
    else
      longestSequence(fullString.substring(0,fullString.lastIndexOf(chars)),fullString)
  }
  def toCipher(encoded: Char, original: Char): Char = {
    val indexEncoded = alphabet.indexOf(encoded)
    val indexOriginal = alphabet.indexOf(original)
    if(indexEncoded >= indexOriginal)
      alphabet.charAt(indexEncoded-indexOriginal)
    else
     alphabet.charAt(26 + (indexEncoded - indexOriginal))
  }

  def repeatKeyWord(keyword:String, message:String):String =
    if(keyword.length() > message.length()) keyword.substring(0, message.length())
    else repeatKeyWord(keyword + keyword, message)

  def offsetAlphabet(cipher:String):String = {
    val index = alphabet.indexOf(cipher)
    val start = alphabet.substring(0, index ).mkString
    val tail = alphabet.substring(index).mkString
    (tail + start)
  }

}
