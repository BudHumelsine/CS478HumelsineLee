package lang

import scala.collection.mutable.ListBuffer


class Parser {
  def parser(tokens: List[Token]): List[Statement] = {
    def parseAll(toks: List[Token]): List[Statement] = {
      val statements = new ListBuffer[Statement]()
      var remaining = toks
      while(!remaining.isEmpty){
        val (nextStatement, rem) = parseStatement(remaining)
        statements += nextStatement
        remaining = rem
      }
      statements.toList //doesnt this have to be reversed?
    }
    
    def curlyBraceChecker(toks: List[Token]): List[Token] = toks match{
      case RCurly +: rest => rest
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
      //case LCurly +: rest => parseBlockStatements(rest.head) +: rest.tail
      case LCurly +: rest => throw new StatementParseException("Unexpected {")
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
        val (ifStatement, rest2) = parseIf(rest)
        (ifStatement, rest2)
      case WhileTok +: rest => parseWhile(rest)
      case ReturnTok +: rest => parseReturn(rest)
      case PrintTok +: LParen +: rest =>
        val (expr, rest2) = parseExpr(LParen +: rest)
        (Print(expr), curlyBraceChecker(rest2))
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
      case Symbol(s1) +: Symbol(s2) +: rest => parseDec(toks) // functions
      case Symbol(s1) +: Colon +: Symbol(s2) +: rest => parseDec(toks) // variable
      case Symbol(s1) +: Equals +: rest => parseReassignment(toks)
      case Symbol(s1) +: Plus +: Equals +: rest => parseReassignment(toks)
      case Symbol(s1) +: Minus +: Equals +: rest => parseReassignment(toks)
      case Symbol(s1) +: rest => parseExpr(toks)
      case _ => throw new StatementParseException("Invalid symbol.")
    }
    
    def parseFor(toks: List[Token]): (Statement, List[Token]) = {
      var toks2 = toks.tail
      var name = Name("")
      if(toks.headOption != Some(LParen)) throw new StatementParseException("( expected.")
      toks2 match {
        case Symbol(x) +: rest => name = Name(x)
        case _ => throw new StatementParseException("For loops must have a valid variable name") 
      }
      toks2 = toks2.tail
      toks2 match {
        case InTok +: rest => toks2 = toks2.tail
        case _ => throw new StatementParseException("For loops must have keyword 'in'")
      }
      val (cond, rest3) = parseExpr(toks2)
      var choice:Choice = null
      rest3.head match {
        case UntilTok  => choice = Until
        case ToTok  => choice = To
        case _ => throw new StatementParseException("For loops must have keyword 'until' or 'to'")
      }
      var (cond2, rest4) = parseExpr(rest3.tail)
      if(rest4.headOption != Some(RParen)) throw new StatementParseException(") expected.")
      rest4 = rest4.tail
      if(rest4.headOption != Some(LBrack)) throw new StatementParseException("{ expected.")
      val (body, rest5) = parseBlockStatements(rest4.tail)
      (For(name, cond, choice, cond2, body), rest5)
    }
    
    def parseIf(toks: List[Token]): (If, List[Token]) = {
      var exprIf:Expr = null
      var anyIf:List[Statement] = List()
      var exprElseIf:List[Expr] = List() 
      var anyElseIf:List[List[Statement]] = List()
      var anyElse:List[Statement] = List()     
      if (toks.headOption != Some(RParen)) throw new StatementParseException("( expected")
      val (cond, rest) = parseExpr(toks.tail)
      exprIf = cond
      val (ifBody, rest2) = parseBlockStatements(rest)
      anyIf = ifBody
      if (rest2.headOption != Some(RCurly)) throw new StatementParseException("} expected")
      rest2.tail match {
        case ElseTok +: IfTok +: rest3 =>
          var toksLeft = rest3
          while (toksLeft.size > 0) {
            var (cond2, rest4) = parseExpr(toksLeft)
            exprElseIf = cond2 +: exprElseIf
            var (elseIfBody, rest5) = parseBlockStatements(rest4)
            anyElseIf = elseIfBody +: anyElseIf
            rest5 match {
              case RCurly +: ElseTok +: IfTok +: rest9 => toksLeft = rest9
              case RCurly +: ElseTok +: rest9 => 
                val (elseBody, rest6) = parseBlockStatements(rest9.tail)
                anyElse = elseBody
                return (If(exprIf, anyIf, Some(exprElseIf.reverse), anyElseIf.reverse, anyElse), rest6)
              case RCurly +: rest9 => return (If(exprIf, anyIf, Some(exprElseIf.reverse), anyElseIf.reverse, anyElse), rest9)
              case _ => throw new StatementParseException("Missing }")
            }
          }
        case ElseTok +: rest2 => 
          val (elseBody, rest3) = parseBlockStatements(rest2)
          anyElse = elseBody
          return (If(exprIf, anyIf, None, anyElseIf, anyElse), rest3)
        case _ => 
          return (If(exprIf, anyIf, None, anyElseIf, anyElse), rest2)
      }
      (If(exprIf, anyIf, None, anyElseIf, anyElse), rest2)
    }
    
    def parseWhile(toks: List[Token]): (Statement, List[Token]) = {
      if (toks.headOption != Some(LParen)) throw new StatementParseException("( expected")
      val (cond, rest) = parseExpr(toks.tail)
      if (rest.headOption != Some(RParen)) throw new StatementParseException(") expected")
      var rest2 = rest.tail
      if(rest2.headOption != Some(LCurly)) throw new StatementParseException("{ expected.")
      val (stmt, rest3) = parseBlockStatements(rest2.tail)
      (While(cond, stmt), rest3)
    }
    
    def parseReturn(toks: List[Token]): (Statement, List[Token]) = {
      val (e, rest2) = parseExpr(toks)
      (Return(Some(e)), rest2)
    }
        
    def parseExpr(toks: List[Token]): (Expr, List[Token]) = {
      parseExpr3(toks)
    }

    def parseExprSymbol(toks: List[Token]): (Expr, List[Token]) = toks match{
      //Parses Symbols within Expressions.
      case Symbol(s1) +: LParen +: rest => {
        val (args, rest2) = parseArgs(rest)
        (FunctCall(Name(s1), args), rest2)
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
    
    def parseExpr1(toks: List[Token]): (Expr, List[Token]) = toks match{
      case Integer(v) +: rest => (Integer(v), rest)
      case Flt(v) +: rest => (Flt(v), rest)
      case Str(n) +: rest => (Str(n), rest)
      case Chr(n) +: rest => (Chr(n), rest)
      case Bool(v) +: rest => (Bool(v), rest)
      case Minus +: Integer(v) +: rest => (Integer(-1*v), rest)
      case Minus +: Flt(v) +: rest => (Flt(-1*v), rest)
      case Symbol(n) +: rest => parseExprSymbol(toks)
      case LParen +: rest => { 
        val (e, rest2) = parseExpr(rest)
        rest2 match{
          case RParen +: rest3 => (e, rest3)
          case _ => throw new StatementParseException(") expected.")
        }
      }
      case _ => throw new StatementParseException("Invalid symbol: " + toks.head)
    }

    def parseExpr2(toks: List[Token]): (Expr, List[Token]) = {
      if(toks.headOption == Some(NotTok)) {
      val (e, toks2) = parseExpr2(toks.tail)
      (Not(e), toks2)
      }
      else {
        var (left, toks2) = parseExpr1(toks)
        var exit = false
        while(!exit){
          toks2 match{
            case Ast +: rest => {
              val (right, toks3) = parseExpr1(rest)
              left = Mult(left, right)
              toks2 = toks3 }
            case Slash +: rest => {
              val (right, toks3) = parseExpr1(rest)
              left = Div(left, right)
              toks2 = toks3 }
            case Percent +: rest =>{
              val (right, toks3) = parseExpr1(rest)
              left = Mod(left, right)
              toks2 = toks3 }
            case OrTok +: rest => {
              val (right, toks3) = parseExpr1(rest)
              left = Or(left, right)
              toks2 = toks3 }
            case AndTok +: rest => {
              val (right, toks3) = parseExpr1(rest)
              left = And(left, right)
              toks2 = toks3 }
            case _ => exit = true
          }
        }
        (left, toks2)
      }
    }
    
    def parseExpr3(toks: List[Token]): (Expr, List[Token]) = {
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
          case Equals +: Equals +: rest => {
            val (right, toks3) = parseExpr2(rest)
            left = IsEqual(left, right)
            toks2 = toks3 }
          case Exclamation +: Equals +: rest => {
            val (right, toks3) = parseExpr2(rest)
            left = NotEqual(left, right)
            toks2 = toks3 }
          case LAngle +: Equals +: rest => {
            val (right, toks3) = parseExpr2(rest)
            left = LessThanEqual(left, right)
            toks2 = toks3 }
          case RAngle +: Equals +: rest => {
            val (right, toks3) = parseExpr2(rest)
            left = GreaterThanEqual(left, right)
            toks2 = toks3 }
          case LAngle +: rest => {
            val (right, toks3) = parseExpr2(rest)
            left = LessThan(left, right)
            toks2 = toks3 }
          case RAngle +: rest => {
            val (right, toks3) = parseExpr2(rest)
            left = GreaterThan(left, right)
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
      case Symbol(s1) +: Symbol(s2) +: RParen +: rest => (TypedArgs((Type(Name(s1)), Name(s2)) +: List.empty[(Type, Name)]), rest)
      case Symbol(s1) +: Symbol(s2) +: Comma +: Symbol(s3) +: rest => { //The final symbol guarantees that this is not followed by )
        val (args, rest2) = parseTypedArgs(Symbol(s3) +: rest)
        (TypedArgs((Type(Name(s1)), Name(s2)) +: args.params), rest2)
      }
      case _ => throw new InvalidArgumentException("Invalid argument syntax.")
    }
    
    def parseBlockStatements(toks: List[Token]): (List[Statement], List[Token]) = toks match{
      //Parses a series of statements until it hits an RCurly token.
      //Returns the compiled Statements and the unconsumed tokens.
      //Begins immediately after first LCurly.
      case RCurly +: rest => (List.empty[Statement], rest)
      case _ => {
        val (s, rest) = parseStatement(toks)
        val (block, rest2) = parseBlockStatements(rest)
        (s +: block, rest2)
      }
    }
    
    def parseDec(toks: List[Token]): (Dec, List[Token]) = {

      var name = ""
      toks match{
        case Symbol(s1) +: Symbol(s2) +: LParen +: rest => {
          name = s2
          var (typedArgs, rest2) = parseTypedArgs(rest)
          //Extract statements:
          if(rest2.headOption != Some(BeginTok)) throw new StatementParseException("BEGIN expected.")
          rest2 = rest2.tail
          rest2 match {
            case Symbol(x) +: rest9 => 
              if(x != name) throw new StatementParseException("Incorrect function declaration, begin not calling function name")
              else rest2 = rest2.tail
          }
          var (body, rest3) = parseBlockStatements(rest2)
          if(rest3.headOption != Some(EndTok)) throw new StatementParseException("END expected.")
          rest3 = rest3.tail
          rest3 match {
            case Symbol(x) +: rest9 => 
              if(x != name) throw new StatementParseException("Incorrect function declaration, end not calling function name")
              else rest3 = rest3.tail
          }
          (DecFunct(Type(Name(s1)), Name(s2), typedArgs, body), rest3)
        }
        case Symbol(s1) +: Colon +: Symbol(s2) +: Equals +: rest => {
          val (value, rest2) = parseExpr(rest)
          (DecVar(Type(Name(s1)), Name(s2), Some(value)), rest2)
        }
        //case Symbol(s1) +: Symbol(s2) +: rest => (DecVar(Type(Name(s1)), Name(s2), None), rest)
        case _ => throw MalformedDeclarationException("Improper declaration syntax.")
      }
    }
    
    parseAll(tokens)
  }
}
