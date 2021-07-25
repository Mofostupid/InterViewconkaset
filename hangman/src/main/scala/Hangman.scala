import java.io.File
import scala.io.Source
import java.nio.file.{Files, Path}
import scala.collection.immutable.TreeSet
import scala.math.random
import scala.util.Random

object Hangman {

  var missingLetters = ""
  var missingword = ""
  var scoreAll = 0
  var score = 0
  var wrong = 10
  var guess: Set[Char] = Set()
  var wrongGuess: Set[Char] = Set()
  var word = ""

  def main(args: Array[String]): Unit = {

    // อ่านไฟล์ เเละเเยกหมวดหมู่
    val dirName = "src/main/source"
    val listfile = getListOfFiles(dirName)
    var wordList: List[String] = List()
    var groupWord: List[List[String]] = List()
    var section: List[String] = List()
    var sectionMap = 1

    listfile.collect(file => {
      var i = 0
      for (line <- Source.fromFile(file).getLines) {
        i > 0 match {
          case true => wordList = line :: wordList
          case false => section = line :: section
        }
        i = i + 1
      }
      groupWord = wordList :: groupWord
      wordList = List()
      sectionMap = sectionMap + 1
    }
    )

    println("Select Catagory:")
    println
    section.collect(section => println(section))
    println

    val choice = scala.io.StdIn.readInt()

    var getWord = Random.shuffle(groupWord(choice - 1)).head
    val hint = getWord.split(",")(1)
    word = getWord.split(",")(0)

    println(s"Hint : ${hint}")
    println

    PrepareGame
    Game

    println(s"answer is ${word} | toTal score : ${scoreAll}" )

    //    game(word)

  }

  def PrepareGame: Unit = {

    val s = word.toList
    s.collect(w =>
      if (w == ' ') {
        missingLetters += w
      } else if (w.isDigit || !w.isASCIILetter) {
        missingLetters += w
      } else {
        missingLetters += '_'
        missingword += w
      }
    )
    score = 100 / missingword.length
  }
  def Game: Unit = {

    var message = missingLetters + s" score : ${scoreAll} , remaining wrong guess : ${wrong}"
    if (!wrongGuess.isEmpty) {
      message += s" , wrong guess : ${wrongGuess.mkString(",")}"
    }

    println(message)
    val input = scala.io.StdIn.readLine
    if(checkInputword(input)) {
      //      input(0).toLower
      //    }
      //    if(!ans.isWhitespace) {
      val ans = input(0).toLower
      if(missingLetters.contains(ans)){
        println(s"you already answer this Charactor : ${input(0)}" )
        wrong -= 1
      }
      else if(word.toLowerCase.contains(ans)){
        val index = 0.until(word.length).filter(word.toLowerCase.startsWith(ans.toString, _)).toList
        missingLetters = replaceCharUsingCharArray(missingLetters, ans, index)
        guess += ans
      } else {
        wrong -= 1
        wrongGuess += ans
      }

    }else {
      if(input != "" && wrongGuess.contains(input(0))) println(s"you already answer this Charactor : ${input(0)}" )
      println("Input Invalid please try again!!")
      wrong -= 1
    }

    if(!CheckmissingLetters.isEmpty && wrong > 0 ) {Game} else {
      if(win) println("congratulations!!")  else println("You lose unfortunately!!")
    }

  }

  def checkInputword(input : String) : Boolean = input != "" && input.length == 1 && !isAllDigits(input) && input(0).isASCIILetter && !wrongGuess.contains(input(0))
  def win :Boolean = CheckmissingLetters.isEmpty
  def CheckmissingLetters: Set[Char] = missingword.toLowerCase.toSet.diff(guess)
  def getListOfFiles(dir: String): List[String] = {
    val file = new File(dir)
    file.listFiles.filter(_.isFile)
      .filter(_.getName.endsWith(".txt"))
      .map(_.getPath).toList
  }

  def replaceCharUsingCharArray(str: String, ch: Char, index: List[Int]): String = {
    val chars = str.toCharArray
    var replaceMissing = ""
    index.collect(index => {
      chars(index) = ch
      replaceMissing = String.valueOf(chars)
      scoreAll += score
    })

    replaceMissing
  }
  def isAllDigits(x: String) = x.matches("^\\d+$")

  implicit class CharProperties(val ch: Char) extends AnyVal {
    def isASCIILetter: Boolean = (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z')
  }
}