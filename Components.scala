package lang


//Tokens
sealed trait Token
case object LParen extends Token // (
case object RParen extends Token // )
case object LCurly extends Token // {
case object RCurly extends Token // }
case object LAngle extends Token //<
case object RAngle extends Token //>
case object LBracket extends Token // [
case object RBracket extends Token // ]
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
case class Integer(value: Int) extends Token with Expr with Value 
case class Flt(value: Float) extends Token with Expr with Value
case class Str(name: String) extends Token with Expr with Value 
case class Chr(name: Char) extends Token with Expr with Value 
case class Bool(value: Boolean) extends Token with Expr with Value 
case class Name(name: String) extends Token //catch names for things such as vars or functions

case object AndTok extends Token
case object OrTok extends Token
case object NotTok extends Token
case object ForTok extends Token
case object IfTok extends Token
case object ElseIfTok extends Token
case object ElseTok extends Token
case object WhileTok extends Token
case object PrintTok extends Token
case object ToTok extends Token
case object UntilTok extends Token
case object InTok extends Token

  

//Statements
sealed trait Expr extends Statement
case class ArrExpr(contents: List[Expr]) extends Expr
case class Ident(name: String) extends Expr
case class Pow(base: Expr, exp: Expr) extends Expr
case class Mult(left: Expr, right: Expr) extends Expr
case class Div(left: Expr, right: Expr) extends Expr
case class DivTrunc(left: Expr, right: Expr) extends Expr
case class Mod(left: Expr, right: Expr) extends Expr
case class Add(left: Expr, right: Expr) extends Expr
case class Sub(left: Expr, right: Expr) extends Expr
case class Not(expr: Expr) extends Expr
case class IsEqual(left: Expr, right: Expr) extends Expr
case class NotEqual(left: Expr, right: Expr) extends Expr
case class LessThanEqual(left: Expr, right: Expr) extends Expr
case class GreaterThanEqual(left: Expr, right: Expr) extends Expr
case class LessThan(left: Expr, right: Expr) extends Expr
case class GreaterThan(left: Expr, right: Expr) extends Expr
case class And(left: Expr, right: Expr) extends Expr
case class Or(left: Expr, right: Expr) extends Expr
case class FunctCall(child: Option[Expr], ident: Ident, args: Args) extends Expr
case class Field(child: Option[Expr], ident: Ident) extends Expr

sealed trait Dec extends Statement
case class DecVar(t: Type, i: Ident, value: Option[Expr] = None) extends Dec
case class DecFunct(t: Type, i: Ident, args: TypedArgs, body: BlockStatement) extends Dec
  
sealed trait Statement
case class Output(expr: Expr) extends Statement
case class Reassign(variable: Ident, expr: Expr) extends Statement
case class BlockStatement(stmts: List[Statement]) extends Statement
case class If(condition: Expr, body: Statement, otherwise: Option[If]) extends Statement
case class While(condition: Expr, body: Statement) extends Statement
case class For(dec: Option[DecVar], condition: Expr, change: Reassign, body: Statement) extends Statement
case class Return(value: Option[Expr] = None) extends Statement
case object Break extends Statement
case object Continue extends Statement

case class Type(ident: Ident, params: Option[List[Type]] = None)
case class Args(params: List[Expr])
case class TypedArgs(params: List[(Type, Ident)])


//Custom Exceptions: should implement more later if time.
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
//Already declared:
//case class Ent(value: Integer) extends Token with Expr with Value//the name Int is taken
//case class Flt(value: Float) extends Token with Expr with Value
//case class Str(name: String) extends Token with Expr with Value //matches items between " and "
//case class Chr(name: Char) extends Token with Expr with Value //matches a single item between ' and '
//case class Bool(value: Boolean) extends Token with Expr with Value //matches true or false
case class FunctVal(t: Type, args: TypedArgs, body: BlockStatement, var staticEnv: Map[String, Location]) extends Value
case class Arr(buf: scala.collection.mutable.ArrayBuffer[Value]) extends Value