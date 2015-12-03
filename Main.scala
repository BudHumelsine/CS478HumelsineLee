package lang

import scala.io.Source


object Main {  
  def main(args: Array[String]){    
    //runProgram("src/files/sample.txt")
    //new Interpreter().runTests
  }
/*
  def runProgram(filename: String){
    val text = getTextFromFile(filename)
    val tokens = scan(text)
    //val parsed = parse(tokens)
    //println(parsed)
    //val interpreted = interpret(parsed)
    //println(interpreted)
  }
  
  def getTextFromFile(filename: String): String = {
    Source.fromFile(filename).getLines.mkString 
  }
  
  def scan(text: String): List[Token] = {
    new Scanner().scanner(text)
  }
  
  def parse(tokens: List[Token]): List[Statement] = {
    new Parser().parser(tokens)
  }
  
  def interpret(program: List[Statement]): List[Value] = {
    new Interpreter().interpret(program)
  }
  */
}