package lang


//Tokens

/*
Kosta, Leonard. CDT F4.  Assistance Given to the Author, electronic copying.  This code for the project was based off the original work done in the official group for this project between CDT Kosta and CDT Lee.
While our code was based on the code from CDTs Lee and Kosta's group, the language for our group is different, and thus major changes were made. 
West Point, NY 9 DEC 15.
*/


sealed trait Token
case object LParen extends Token // (
case object RParen extends Token // )
case object LCurly extends Token // {
case object RCurly extends Token // }
case object LAngle extends Token //<
case object RAngle extends Token //>
case object LBrack extends Token // [
case object RBrack extends Token // ]
case object Comma extends Token
case object Dot extends Token
case object Slash extends Token
case object Plus extends Token
case object Minus extends Token
case object Percent extends Token
case object Ast extends Token
case object Exclamation extends Token
case object Equals extends Token
case object Colon extends Token
case object FieldTok extends Token
case class Integer(value: Int) extends Token with Expr with Value 
case class Flt(value: Float) extends Token with Expr with Value
case class Str(name: String) extends Token with Expr with Value 
case class Chr(name: Char) extends Token with Expr with Value 
case class Bool(value: Boolean) extends Token with Expr with Value 
case class Name(name: String) extends Token with Expr 
case class Symbol(name:String) extends Token with Value

case object AndTok extends Token
case object OrTok extends Token
case object NotTok extends Token
case object ForTok extends Token
case object IfTok extends Token
case object ElseIfTok extends Token
case object ElseTok extends Token
case object ReturnTok extends Token
case object WhileTok extends Token
case object PrintTok extends Token
case object ToTok extends Token 
case object UntilTok extends Token
case object InTok extends Token
case object BeginTok extends Token
case object EndTok extends Token
case object ArrayTok extends Token

 
//Statements
sealed trait Expr extends Statement
case class ArrExpr(contents: List[Expr]) extends Expr
case class Mult(left: Expr, right: Expr) extends Expr
case class Div(left: Expr, right: Expr) extends Expr
case class Mod(left: Expr, right: Expr) extends Expr
case class Add(left: Expr, right: Expr) extends Expr
case class Sub(left: Expr, right: Expr) extends Expr
case class Not(expr: Expr) extends Expr
case class MyArray(arr:Array[Integer]) extends Expr
case class IsEqual(left: Expr, right: Expr) extends Expr
case class NotEqual(left: Expr, right: Expr) extends Expr
case class LessThanEqual(left: Expr, right: Expr) extends Expr
case class GreaterThanEqual(left: Expr, right: Expr) extends Expr
case class LessThan(left: Expr, right: Expr) extends Expr
case class GreaterThan(left: Expr, right: Expr) extends Expr
case class And(left: Expr, right: Expr) extends Expr
case class Or(left: Expr, right: Expr) extends Expr
case class FunctCall(name: Name, args: Args) extends Expr

sealed trait Dec extends Statement
case class DecVar(t: Type, i: Name, value: Option[Expr] = None) extends Dec
case class DecFunct(t: Type, i: Name, args: TypedArgs, body: List[Statement]) extends Dec
case class DecField(n:Name, vals:List[DecVar]) extends Dec
  
sealed trait Statement
case class Print(expr: Expr) extends Statement
case class Reassign(variable: Name, expr: Expr) extends Statement
case class For(name: Name, expr:Expr, choice:Choice, expr2:Expr, l:List[Statement]) extends Statement
case class While(expr: Expr, l:List[Statement]) extends Statement 
case class If(exprIf:Expr, anyIf: List[Statement], exprElseIf:Option[List[Expr]], anyElseIf:List[List[Statement]], anyElse: List[Statement]) extends Statement
case class Return(value: Option[Expr] = None) extends Statement

case class Type(name: Name, params: Option[List[Type]] = None)
case class Args(params: List[Expr])
case class TypedArgs(params: List[(Type, Name)])


sealed trait Choice
case object To extends Choice
case object Until extends Choice



//Custom Exceptions
//The following custom exceptions are directly copied from the suite of custom exceptions for Lee-Kosta's exisiting project.
case class MalformedDeclarationException(msg: String) extends Exception(msg)
case class StringScanException(msg: String) extends Exception(msg)
case class CharScanException(msg: String) extends Exception(msg)
case class InvalidCharacterException(msg: String) extends Exception(msg)
case class InvalidArgumentException(msg: String) extends Exception(msg)
case class StatementParseException(msg: String) extends Exception(msg)
case class NotImplementedException(msg: String) extends Exception(msg)
case class UnboundVariableException(msg: String) extends Exception(msg)
case class InvalidStatementException(msg: String) extends Exception(msg)
case class InvalidExpressionException(msg: String) extends Exception(msg)
case class ConversionException(msg: String) extends Exception(msg)

class Location(t: Type, value: Option[Value]){
  var typ = t
  var contents = value
}
//Values
sealed trait Value
case class FunctVal(t: Type, args: TypedArgs, body: List[Statement], var staticEnv: Map[String, Location]) extends Value
case class Arr(buf: scala.collection.mutable.ArrayBuffer[Value]) extends Value

