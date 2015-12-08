package lang

import scala.collection.mutable.ListBuffer


class Parser {
  def parser(tokens: List[Token]): List[Statement] = {
    def parseAll(toks: List[Token]): List[Statement] = {
      //Converts all tokens into statements.
      //Will not quit until all tokens are consumed.
      val statements = new ListBuffer[Statement]()
      var remaining = toks
      while(!remaining.isEmpty){
        val (nextStatement, rem) = parseStatement(remaining) //(Statement, List[Token])
        statements += nextStatement
        remaining = rem
      }
      statements.toList
    }
    
    def curlyBraceChecker(toks: List[Token]): List[Token] = toks match{
      //Check for a curlybrace token at the end of a statement.
      //Return the tokens after the curlybrace.
      case RCurly +: rest => rest //this is correct; do nothing.
      case _ => throw new StatementParseException("} expected.")
    }
    
    def parseStatement(toks: List[Token]): (Statement, List[Token]) = toks match{
      //Returns the statement that results from the input, and a List of the unconsumed Tokens.
      //If a statement begins with a token not described in the CFG, throws Exception.
      case BeginTok +: rest => 
        val (stmt, rest2) = parseExpr(LParen +: rest)
        (stmt, curlyBraceChecker(rest2))
      case LParen +: rest => 
        val (stmt, rest2) = parseExpr(LParen +: rest)
        (stmt, curlyBraceChecker(rest2))
      case LBrack +: rest => 
        val (stmt, rest2) = parseExpr(LBrack +: rest)
        (stmt, curlyBraceChecker(rest2))
      case LCurly +: rest => parseBlockStatements(rest)
      case Minus +: rest => //Could begin a negative number
        val (stmt, rest2) = parseExpr(Minus +: rest)
        (stmt, curlyBraceChecker(rest2))
      case Integer(v) +: rest => 
        val (stmt, rest2) = parseExpr(Integer(v) +: rest)
        (stmt, curlyBraceChecker(rest2))
      case Flt(v) +: rest => 
        val (stmt, rest2) = parseExpr(Flt(v) +: rest)
        (stmt, curlyBraceChecker(rest2))
      case Str(n) +: rest => 
        val (stmt, rest2) = parseExpr(Str(n) +: rest)
        (stmt, curlyBraceChecker(rest2))
      case Chr(n) +: rest => 
        val (stmt, rest2) = parseExpr(Chr(n) +: rest)
        (stmt, curlyBraceChecker(rest2))
      case Bool(v) +: rest =>
        val (stmt, rest2) = parseExpr(Bool(v) +: rest)
        (stmt, curlyBraceChecker(rest2))
      //Be careful with Symbol.
      //Could begin an expression, a declaration, or a reassignment statement.
      case Symbol(n) +: rest =>
        val (stmt, rest2) = parseSymbol(Symbol(n) +: rest)
        (stmt, curlyBraceChecker(rest2))
      case NotTok +: rest => 
        val (stmt, rest2) = parseExpr(NotTok +: rest)
        (stmt, curlyBraceChecker(rest2))
      case ForTok +: rest => parseFor(rest)
      case IfTok +: rest => 
        val (ifStatement, rest2) = parseIf(IfTok +: rest)
        (ifStatement, rest2)
      case WhileTok +: rest => parseWhile(rest)
      case ReturnTok +: rest => parseReturn(rest)
      case PrintTok +: LParen +: rest =>
        val (expr, rest2) = parseExpr(LParen +: rest)
        (Output(expr), curlyBraceChecker(rest2))
      //Anything else is an invalid start of a statement.
      case _ => throw new StatementParseException("Invalid symbol: "+toks.head)
    }
    
    def parseReassignment(toks: List[Token]): (Statement, List[Token]) = toks match{
      //Parses reassignment.
      case Symbol(s1) +: Equals +: rest => {
        val (body, rest2) = parseExpr(rest)
        (Reassign(Name(s1), body), rest2)
      }      
      case Symbol(s1) +: Plus +: Equals +: rest => {
        val (body, rest2) = parseExpr(rest)
        (Reassign(Name(s1), Add(Name(s1), body)), rest2)
      }      
      case Symbol(s1) +: Minus +: Equals +: rest => {
        val (body, rest2) = parseExpr(rest)
        (Reassign(Name(s1), Sub(Name(s1), body)), rest2)
      } 
      case _ => throw new StatementParseException("Invalid symbol: " + toks.head)
    }
    
    def parseSymbol(toks: List[Token]): (Statement, List[Token]) = toks match{
      //Parses a Symbol.
      //toks begins with a Symbol.
      //A Symbol may begin an expression, a declaration, or a reassignment.
      //Luckily, these are mutually exclusive definitions.
      case Symbol(s1) +: Symbol(s2) +: rest => parseDec(toks)
      case Symbol(s1) +: Equals +: rest => parseReassignment(toks)
      case Symbol(s1) +: Plus +: Equals +: rest => parseReassignment(toks)
      case Symbol(s1) +: Minus +: Equals +: rest => parseReassignment(toks)
      case Symbol(s1) +: rest => parseExpr(toks)
      case _ => throw new StatementParseException("Invalid symbol.")
    }
    
    def parseFor(toks: List[Token]): (Statement, List[Token]) = {
      //Parses a For statement.
      //Begins after ForTok.
      var (dec, rest2) = (None: Option[DecVar], toks)
      try{
        val (dec0, rest0) = parseDec(toks)
        dec = Some(dec0.asInstanceOf[DecVar])
        rest2 = rest0
        if(rest2.headOption != Some(Comma)) throw new StatementParseException(", expected.")
        rest2 = rest2.tail
      }
      catch{
        case MalformedDeclarationException(msg) => null //there is no Dec at the beginning of the statement.
      }
      val (cond, rest3) = parseExpr(rest2)
      if(rest3.headOption != Some(Comma)) throw new StatementParseException(", expected.")
      val (reassign, rest4) = parseReassignment(rest3.tail)
      if(rest4.headOption != Some(LCurly)) throw new StatementParseException("{ expected.")
      val (body, rest5) = parseBlockStatements(rest4.tail)
      (For(dec, cond, reassign.asInstanceOf[Reassign], body), rest5)
    }
    
    def parseIf(toks: List[Token]): (If, List[Token]) = toks match{
      //Parses an If statement.
      //Begins with the IfTok.
      case IfTok +: rest => {
        val (cond, rest2) = parseExpr(rest)
        if(rest2.headOption != Some(LCurly)) throw new StatementParseException("Improper if syntax. ( expected.")
        val (body, rest3) = parseBlockStatements(rest2.tail)
        if(rest3.headOption == Some(ElifTok) || rest3.headOption == Some(ElseTok)) {
          val (otherwise, rest4) = parseIf(rest3)
          (If(cond, body, Some(otherwise)), rest4)
        }
        else (If(cond, body, None), rest3)
      }
      case ElifTok +: rest => {
        val (cond, rest2) = parseExpr(rest)
        if(rest2.headOption != Some(LCurly)) throw new StatementParseException("Improper if syntax. ( expected.")
        val (body, rest3) = parseBlockStatements(rest2.tail)
        if(rest3.headOption == Some(ElifTok) || rest3.headOption == Some(ElseTok)) {
          val (otherwise, rest4) = parseIf(rest3)
          (If(cond, body, Some(otherwise)), rest4)
        }
        else (If(cond, body, None), rest3)
      }
      case ElseTok +: rest => {
        if(rest.headOption != Some(LCurly)) throw new StatementParseException("Improper if syntax. ( expected.")
        val (body, rest2) = parseBlockStatements(rest.tail)
        (If(Bool(true), body, None), rest2)
      }
      case _ => throw new StatementParseException("Invalid symbol.")
    }
    
    def parseWhile(toks: List[Token]): (Statement, List[Token]) = {
      //Parses a While statement.
      //Begins with the conditional.
      val (cond, rest) = parseExpr(toks)
      if(rest.headOption != Some(LCurly)) throw new StatementParseException("Improper while syntax. ( expected.")
      val (stmt, rest2) = parseBlockStatements(rest.tail)
      (While(cond, stmt), rest2)
    }
    
    def parseReturn(toks: List[Token]): (Statement, List[Token]) = toks match{
      //Parses a Return statement.
      //Begins after the ReturnTok.
      case SemiColon +: rest => (Return(None), rest)
      case _ => {
        val (e, rest2) = parseExpr(toks)
        rest2 match{
          case SemiColon +: rest3 => (Return(Some(e)), rest3) //this is correct; do nothing.
          case _ => throw new StatementParseException("; expected.")
        }
      }
    }
        
    def parseExpr(toks: List[Token]): (Expr, List[Token]) = {
      //Parses an Expr from the front of toks and returns that Expr and the unconsumed Tokens.
      parseExpr7(toks)
    }
    
    def parseExprSymbol(toks: List[Token]): (Expr, List[Token]) = toks match{
      //Parses Symbols within Expressions.
      case Symbol(s1) +: LParen +: rest => {
        val (args, rest2) = parseArgs(rest)
        rest2 match{
          case Dot +: Symbol(s2) +: rest3 => {
            val (child, rest4) = parseExprSymbol(rest2.tail)
            (FunctCall(Some(child), Name(s1), args), rest4)
          }
          case _ => (FunctCall(None, Name(s1), args), rest2)
        }
      }
      case Symbol(s1) +: rest => {
        rest match{
          case Dot +: Symbol(s2) +: rest3 => {
            val (child, rest4) = parseExprSymbol(rest.tail)
            (Field(Some(child), Name(s1)), rest4)
          }
          case _ => (Field(None, Name(s1)), rest)
        }
      }
      case _ => throw new StatementParseException("Invalid symbol: "+toks.head)
    }
    
    def parseArray(toks: List[Token]): (ArrExpr, List[Token]) = toks match{
      //Parses and returns an ArrExpr (implemented with a scala.collection.mutable.ArrayBuffer, the Scala version of Java's ArrayList).
      //Begins after first [.
      case RBrack +: rest => (ArrExpr(List.empty), rest)
      case _ => {
        val (arg, rest) = parseExpr(toks)
        rest match{
          case RBrack +: rest => (ArrExpr(List(arg)), rest)
          case Comma +: rest => {
            val (arr, rest2) = parseArray(rest)
            (ArrExpr(arg +: arr.contents), rest2)
          }
        }
      }
    }
    
    def parseExpr0(toks: List[Token]): (Expr, List[Token]) = toks match{
      //Atomic expressions.
      case Integer(v) +: rest => (Integer(v), rest)
      case Flt(v) +: rest => (Flt(v), rest)
      case Str(n) +: rest => (Str(n), rest)
      case Chr(n) +: rest => (Chr(n), rest)
      case Bool(v) +: rest => (Bool(v), rest)
      case Minus +: Integer(v) +: rest => (Integer(-1*v), rest)
      case Minus +: Flt(v) +: rest => (Flt(-1*v), rest)
      //Symbol will be fully implemented when we begin doing more object-oriented programming.
      case Symbol(n) +: rest => parseExprSymbol(toks)
      case LParen +: rest => { 
        val (e, rest2) = parseExpr(rest)
        rest2 match{
          case RParen +: rest3 => (e, rest3) //this is correct; do nothing.
          case _ => throw new StatementParseException(") expected.")
        }
      }
      case LBrack +: rest => parseArray(rest)
      case _ => throw new StatementParseException("Invalid symbol: " + toks.head)
    }
    
    def parseExpr1(toks: List[Token]): (Expr, List[Token]) = {
      //Exponentiation.
      //Note: exponentiation gets handled right to left.
      //Recursive: <expr1> = <expr0> "**" <expr1> | <expr0>
      //Iterative: <expr1> = <expr0> ("**" <expr1>)*
      val (base, toks2) = parseExpr0(toks)
      toks2 match{
        case Ast +: Ast +: rest =>
          val (exp, toks3) = parseExpr1(toks2.tail.tail)
          (Pow(base, exp), toks3)
        case _ => (base, toks2)
      }
    }
    
    def parseExpr2(toks: List[Token]): (Expr, List[Token]) = {
      //Multiplication, division, divide and truncate, modulo.
      //Recursive: <expr2> = <expr2> "*" <expr1> | <expr1>
      //Iterative: <expr2> = <expr1> ("*" <expr1>)*
      var (left, toks2) = parseExpr1(toks)
      var exit = false
      while(!exit){
        toks2 match{
          case Ast +: rest => {
            val (right, toks3) = parseExpr1(rest)
            left = Mult(left, right)
            toks2 = toks3 }
          case Slash +: Slash +: rest => {
            val (right, toks3) = parseExpr1(rest)
            left = DivTrunc(left, right)
            toks2 = toks3 }
          case Slash +: rest => {
            val (right, toks3) = parseExpr1(rest)
            left = Div(left, right)
            toks2 = toks3 }
          case Percent +: rest =>{
            val (right, toks3) = parseExpr1(rest)
            left = Mod(left, right)
            toks2 = toks3 }
          case _ => exit = true
        }
      }
      (left, toks2)
    }
    
    def parseExpr3(toks: List[Token]): (Expr, List[Token]) = {
      //Addition, subtraction.
      //Recursive: <expr3> = <expr3> "+" <expr2> | <expr2>
      //Iterative: <expr3> = <expr2> ("+" <expr2>)*
      var (left, toks2) = parseExpr2(toks)
      var exit = false
      while(!exit){
        toks2 match{
          case Plus +: rest => {
            val (right, toks3) = parseExpr2(rest)
            left = Add(left, right)
            toks2 = toks3 }
          case Minus +: rest => {
            val (right, toks3) = parseExpr2(rest)
            left = Sub(left, right)
            toks2 = toks3 }
          case _ => exit = true
        }
      }
      (left, toks2)
    }
    
    def parseExpr4(toks: List[Token]): (Expr, List[Token]) = {
      //Not.
      //Note: using the recursive definition here is not problematic since there is no left recursion.
      //Recursive: <expr4> = "not" <expr4> | <expr3>
      //Iterative: <expr4> = ("not")* <expr3>
      if(toks.headOption == Some(NotTok)) {
        val (e, toks2) = parseExpr4(toks.tail)
        (Not(e), toks2)
      }
      else parseExpr3(toks)
    } 
    
    def parseExpr5(toks: List[Token]): (Expr, List[Token]) = {
      //Comparators.
      //Recursive: <expr5> = <expr5> "==" <expr4> | <expr4>
      //Iterative: <expr5> = <expr4> ("==" <expr4>)*
      var (left, toks2) = parseExpr4(toks)
      var exit = false
      while(!exit){
        toks2 match{
          case Equals +: Equals +: rest => {
            val (right, toks3) = parseExpr4(rest)
            left = IsEqual(left, right)
            toks2 = toks3 }
          case Exclamation +: Equals +: rest => {
            val (right, toks3) = parseExpr4(rest)
            left = NotEqual(left, right)
            toks2 = toks3 }
          case LAngle +: Equals +: rest => {
            val (right, toks3) = parseExpr4(rest)
            left = LessThanEqual(left, right)
            toks2 = toks3 }
          case RAngle +: Equals +: rest => {
            val (right, toks3) = parseExpr4(rest)
            left = GreaterThanEqual(left, right)
            toks2 = toks3 }
          case LAngle +: rest => {
            val (right, toks3) = parseExpr4(rest)
            left = LessThan(left, right)
            toks2 = toks3 }
          case RAngle +: rest => {
            val (right, toks3) = parseExpr4(rest)
            left = GreaterThan(left, right)
            toks2 = toks3 }
          case _ => exit = true
        }
      }
      (left, toks2)
    }
    
    def parseExpr6(toks: List[Token]): (Expr, List[Token]) = {
      //And.
      //Recursive: <expr6> = <expr6> "and" <expr5> | <expr5>
      //Iterative: <expr5> = <expr5> ("and" <expr5>)*
      var (left, toks2) = parseExpr5(toks)
      var exit = false
      while(!exit){
        toks2 match{
          case AndTok +: rest => {
            val (right, toks3) = parseExpr5(rest)
            left = And(left, right)
            toks2 = toks3 }
          case _ => exit = true
        }
      }
      (left, toks2)
    }
    
    def parseExpr7(toks: List[Token]): (Expr, List[Token]) = {
      //Or.
      //Recursive: <expr7> = <expr7> "or" <expr6> | <expr6>
      //Iterative: <expr7> = <expr6> ("or" <expr6>)*
      var (left, toks2) = parseExpr6(toks)
      var exit = false
      while(!exit){
        toks2 match{
          case OrTok +: rest => {
            val (right, toks3) = parseExpr6(rest)
            left = Or(left, right)
            toks2 = toks3 }
          case _ => exit = true
        }
      }
      (left, toks2)
    }
    
    def parseArgs(toks: List[Token]): (Args, List[Token]) = toks match{
      //Parses a series of arguments until it hits an RParen token.
      //Returns the compiled Args and the unconsumed tokens.
      case RParen +: rest => (Args(List.empty[Expr]), rest)
      case _ => {
        val (arg, rest) = parseExpr(toks)
        rest match{
          case RParen +: rest => (Args(List(arg)), rest)
          case Comma +: rest => {
            val (args, rest2) = parseArgs(rest)
            (Args(arg +: args.params), rest2)
          }
        }
      }
    }
    
    def parseTypedArgs(toks: List[Token]): (TypedArgs, List[Token]) = toks match{
      //Parses a series of typed arguments until it hits an RParen token.
      //Returns the compiled TypedArgs and the unconsumed tokens.
      case RParen +: rest => (TypedArgs(List.empty[(Type, Name)]), rest)
      case Symbol(s1) +: Symbol(s2) +: RParen +: rest => (TypedArgs((Type(Name(s1)), Names2)) +: List.empty[(Type, Name)]), rest)
      case Symbol(s1) +: Symbol(s2) +: Comma +: Symbol(s3) +: rest => { //The final symbol guarantees that this is not followed by )
        val (args, rest2) = parseTypedArgs(Symbol(s3) +: rest)
        (TypedArgs((Type(Name(s1)), Name(s2)) +: args.params), rest2)
      }
      case _ => throw new InvalidArgumentException("Invalid argument syntax.")
    }
    
    def parseBlockStatements(toks: List[Token]): (BlockStatement, List[Token]) = toks match{
      //Parses a series of statements until it hits an RCurly token.
      //Returns the compiled Statements and the unconsumed tokens.
      //Begins immediately after first LCurly.
      case RCurly +: rest => (BlockStatement(List.empty[Statement]), rest)
      case _ => {
        val (s, rest) = parseStatement(toks)
        val (block, rest2) = parseBlockStatements(rest)
        (BlockStatement(s +: block.stmts), rest2)
      }
    }
    
    def parseDec(toks: List[Token]): (Dec, List[Token]) = toks match{
      //Parses a Dec according to the CFG and returns that Dec and a list of the unconsumed tokens.
      //A correctly formed Dec is expected; if one is not found, this will throw an exception.
      //DecFunct(Type(s1), Name(s2), args, body)
      case Symbol(s1) +: Symbol(s2) +: LParen +: rest => {
        //Extract typed args:
        val (typedArgs, rest2) = parseTypedArgs(rest)
        //Extract statements:
        if(rest2.headOption != Some(LCurly)) throw new StatementParseException("{ expected.")
        val (body, rest3) = parseBlockStatements(rest2.tail)
        (DecFunct(Type(Name(s1)), Name(s2), typedArgs, body), SemiColon +: rest3)
      } 
      case Symbol(s1) +: Symbol(s2) +: Equals +: NewTok +: Symbol(s3) +: LParen +: rest => throw new NotImplementedException("Object oriented functionality not yet implemented.")
      case Symbol(s1) +: Symbol(s2) +: Equals +: rest => {
        val (value, rest2) = parseExpr(rest)
        (DecVar(Type(Name(s1)), Name(s2), Some(value)), rest2)
      }
      case Symbol(s1) +: Symbol(s2) +: rest => (DecVar(Type(Name(s1)), Name(s2), None), rest)
      case _ => throw MalformedDeclarationException("Improper declaration syntax.")
    }
    
    parseAll(tokens)
  }
}
