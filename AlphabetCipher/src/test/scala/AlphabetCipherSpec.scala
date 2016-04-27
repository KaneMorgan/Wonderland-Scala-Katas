import AlphabetCipher._
import org.scalatest
import org.scalatest.{FlatSpec, Matchers}

class AlphabetCipherSpec extends FlatSpec with Matchers {

  "repeatKeyWord" should "return the keyword repeated to match the method length" in {
    assert(repeatKeyWord("four","one") == "fou")
    assert(repeatKeyWord("one","four") == "oneo")
  }

  "offsetAlphabet" should "return the correct characters" in {
    assert(offsetAlphabet("b") == "bcdefghijklmnopqrstuvwxyza")
  }

  "encodeChar" should "encode characters with the cipher" in {
    assert(encodeCharacter("v","m") == "h")
  }

  "decodeChar" should "decode characters with the cipher" in {
    assert(decodeCharacter("v","h") == "m")
  }

  "encode" should "encode given a secret keyword" in {
    assert(encode("vigilance","meetmeontuesdayeveningatseven") == "hmkbxebpxpmyllyrxiiqtoltfgzzv")
    assert(encode("scones","meetmebythetree") == "egsgqwtahuiljgs")
  }

  "decode" should "decode an cyrpted message given a secret keyword" in {
    assert(decode("vigilance","hmkbxebpxpmyllyrxiiqtoltfgzzv") == "meetmeontuesdayeveningatseven")
    assert(decode("scones","egsgqwtahuiljgs") == "meetmebythetree")

  }

  "toCipher" should "convert characters to ciphers" in {
    assert(toCipher('o','t') == 'v')
  }

  "decipher" should "extract the secret keyword given an encrypted message and the original message" in {
    assert(decipher("opkyfipmfmwcvqoklyhxywgeecpvhelzg", "thequickbrownfoxjumpsoveralazydog") == "vigilance")
    assert(decipher("hcqxqqtqljmlzhwiivgbsapaiwcenmyu", "packmyboxwithfivedozenliquorjugs") == "scones")
  }
}