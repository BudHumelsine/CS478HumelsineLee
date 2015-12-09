package lang


import scala.io.Source
//notes
//to run
// >>>scalac *.scala
// >>>scala lang.Main


object Main {  
  def main(args: Array[String]){  
    runProgram("phase1.txt")
    //new Interpreter().runTests
  }

  def runProgram(filename: String){
    val text = getTextFromFile(filename)
    val tokens = scan(text)
    //println(tokens) //debug only
    val parsed = parse(tokens)
    println(parsed) //debug only
    val interpreted = interpret(parsed)
    //println(interpreted)
  }
  
  def getTextFromFile(filename: String): String = {
    Source.fromFile(filename).mkString 
    //Source.fromFile(filename).getLines.mkString 
  }
  
  def scan(text: String): List[Token] = {
    new Scanner().scanner(text)
  }
 
  def parse(tokens: List[Token]): List[Statement] = {
    new Parser().parser(tokens)
  }
  
  def interpret(program: List[Statement]): Unit = {
    new Interpreter().interpret(program)
  }
}
