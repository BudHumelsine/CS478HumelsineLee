package lang

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer


class Interpreter {
  type Env = Map[String, Location]
  
  def interpret(program: List[Statement]): Unit = {
    var returnVal = None: Option[Value]
    
    def arrayMult(arr: Arr, n: Int): Arr = {
      //Copies this array n times.
      //Returns a new Arr.
      if(n < 0) throw new InvalidExpressionException("Cannot multiply Array by negative integer.")
      else if(n == 0) Arr(ArrayBuffer.empty)
      else if(n == 1) Arr(arr.buf)
      else Arr(arr.buf ++ arrayMult(arr, n-1).buf)
    }
    
    def eval(expr: Expr, env: Env): Value = expr match {
      case Integer(v) => Integer(v)
      case Flt(v) => Flt(v)
      case Str(s) => Str(s)
      case Chr(c) => Chr(c)
      case Bool(b) => Bool(b)
      case ArrExpr(list) =>
        val buf = list.to[scala.collection.mutable.ArrayBuffer].map(e => eval(e, env))
        Arr(buf)
      case Name(name) =>
        val prev = env.get(name)
        prev match{
          case Some(loc) =>
            if(loc.contents == None) null
            else loc.contents.get
          case None => throw new UnboundVariableException("Variable " + name + " does not have a binding.")
        }
      case Mult(x, y) =>
        val ex = eval(x, env)
        val ey = eval(y, env)
        (ex, ey) match{
          case (Arr(b1), Integer(v)) => arrayMult(Arr(b1), v)
          case (Integer(v), Arr(b1)) => arrayMult(Arr(b1), v)
          //If both are Integers, keep as Integer. Otherwise convert to Flt.
          case (Integer(n1), Integer(n2)) => Integer(valToInt(ex) * valToInt(ey))
          case (Flt(n1), Integer(n2)) => Flt(valToFloat(ex) * valToFloat(ey))
          case (Integer(n1), Flt(n2)) => Flt(valToFloat(ex) * valToFloat(ey))
          case (Flt(n1), Flt(n2)) => Flt(valToFloat(ex) * valToFloat(ey))
          case _ => throw new InvalidExpressionException("Unsupported operation: " + ex + " * " + ey + ".")
        }
      case Div(x, y) =>
        val ex = eval(x, env)
        val ey = eval(y, env)
        //If both are Integers, keep as Integer. Otherwise convert to Flt.
        if(ex.isInstanceOf[Integer] && ey.isInstanceOf[Integer]) Integer(valToInt(ex) / valToInt(ey))
        else Flt(valToFloat(ex) / valToFloat(ey))
      case DivTrunc(x, y) =>
        val ex = eval(x, env)
        val ey = eval(y, env)
        if(ex.isInstanceOf[Integer] && ey.isInstanceOf[Integer]) Integer(valToInt(ex) / valToInt(ey))
        else Flt(math.floor(valToFloat(ex) / valToFloat(ey)).toFloat)
      case Mod(x, y) =>
        val ex = eval(x, env)
        val ey = eval(y, env)
        if(ex.isInstanceOf[Integer] && ey.isInstanceOf[Integer]) Integer(valToInt(ex) % valToInt(ey))
        else Flt(valToFloat(ex) % valToFloat(ey))
      case Add(x, y) =>
        val ex = eval(x, env)
        val ey = eval(y, env)
        (ex, ey) match{
          case (Arr(b1), Arr(b2)) => Arr(b1 ++ b2) //join
          case (Arr(b1), v) => Arr(b1 :+ v) //append
          case (v, Arr(b1)) => Arr(v +: b1) //prepend
          //If both are Integers, keep as Integer. Otherwise convert to Flt.
          case (Integer(n1), Integer(n2)) => Integer(valToInt(ex) + valToInt(ey))
          case (Flt(n1), Integer(n2)) => Flt(valToFloat(ex) + valToFloat(ey))
          case (Integer(n1), Flt(n2)) => Flt(valToFloat(ex) + valToFloat(ey))
          case (Flt(n1), Flt(n2)) => Flt(valToFloat(ex) + valToFloat(ey))
          case _ => throw new InvalidExpressionException("Unsupported operation: " + ex + " + " + ey + ".")
        }
      case Sub(x, y) =>
        val ex = eval(x, env)
        val ey = eval(y, env)
        //If both are Integers, keep as Integer. Otherwise convert to Flt.
        if(ex.isInstanceOf[Integer] && ey.isInstanceOf[Integer]) Integer(valToInt(ex) - valToInt(ey))
        else Flt(valToFloat(ex) - valToFloat(ey))
      case Not(e) => Bool(!valToBool(eval(e, env)))
      case IsEqual(x, y) => Bool(eval(x, env) == eval(y, env))
      case NotEqual(x, y) => Bool(eval(x, env) != eval(y, env))
      case LessThanEqual(x, y) => Bool(valToFloat(eval(x, env)) <= valToFloat(eval(y, env)))
      case GreaterThanEqual(x, y) => Bool(valToFloat(eval(x, env)) >= valToFloat(eval(y, env)))
      case LessThan(x, y) => Bool(valToFloat(eval(x, env)) < valToFloat(eval(y, env)))
      case GreaterThan(x, y) => Bool(valToFloat(eval(x, env)) > valToFloat(eval(y, env)))
      case And(x, y) => Bool(valToBool(eval(x, env)) && valToBool(eval(y, env)))
      case Or(x, y) => Bool(valToBool(eval(x, env)) || valToBool(eval(y, env)))
      case FunctCall(name, args) =>
        val prev = env.get(name.name)
        prev match{
          case Some(loc) =>
            //Make sure loc is storing a function.
            loc.contents match{
              case Some(FunctVal(t, a, body, staticEnv)) =>
                if(args.params.length != a.params.length) throw new InvalidStatementException("Expected " + a.params.length + " args, but got " + args.params.length)
                execFunct(FunctVal(t, a, body, staticEnv), args, env)
              case _ => throw new InvalidStatementException(loc.contents + " is not a function.")
            }
          case None => throw new UnboundVariableException("Function " + name.name + " does not have a binding.")
        }
      case _ => throw new InvalidExpressionException("Invalid expression: " + expr)
    }
    
    def valToBool(v: Value): Boolean = v match {
      case Bool(x) => x
      case _ => throw new ConversionException("Cannot convert " + v + " to Boolean")
    }
    
    def valToInt(v: Value): Int = v match {
      case Integer(x) => x
      case Flt(x) => x.toInt
      case _ => throw new ConversionException("Cannot convert " + v + " to Int")
    }
    
    def valToFloat(v: Value): Float = v match {
      case Integer(x) => x.toFloat
      case Flt(x) => x
      case _ => throw new ConversionException("Cannot convert " + v + " to Float")
    }
    
    def declare(dec: Dec, env: Env): Env = dec match{
      case DecVar(t, name, value) =>
        if(value == None) env + (name.name -> new Location(t, None))
        else{
          val v = eval(value.get, env) 
          env + (name.name -> new Location(t, Some(v)))
        }
      case DecFunct(t, name, args, body) =>
        val funct = FunctVal(t, args, body, env)
        val newEnv = env + (name.name -> new Location(t, Some(funct)))
        //place this function in its own environment to allow recursion.
        funct.staticEnv = newEnv
        newEnv
    }
    
    def reassign(re: Reassign, env: Env): Env = {
      val v = eval(re.expr, env)
      val prev = env.get(re.variable.name)
      prev match{
        case Some(loc) =>
          loc.contents = Some(v)
          env
        case None => 
          throw new UnboundVariableException("Variable " + re.variable.name + " does not have a binding.")
      }
    }
    
    def exec(stmts: List[Statement], env: Env): Unit = {
      if(!returnVal.isEmpty) return
      stmts match {
      case Print(e) +: rest =>
        println(eval(e, env))
        exec(rest, env)
      case If(exprIf, anyIf, exprElseIf, anyElseIf, anyElse) +: rest =>
        if (valToBool(eval(exprIf, env))) exec(anyIf, env)
        else if (exprElseIf.isDefined) {
          var elifList = exprElseIf.head.toArray
          var elifBody = anyElseIf.head.toArray
          var flag = false
          var elseFlag = false
          while (!flag) {
            for (elem <- 0 until elifList.size) {
              if (elem == elifList.size-1) flag = true
              val toTest = eval(elifList(elem), env)
              if (valToBool(toTest)) {
                flag = true
                elseFlag = true
                exec(List(elifBody(elem)), env)
              }
            }
          }
          if (elseFlag && anyElse.size > 0) exec(anyElse, env)
          exec (rest, env)
        }
      case While(cond, body) +: rest =>
        while(valToBool(eval(cond, env))) exec(body, env)
        exec(rest, env)
      case For(n,e, c, e2, a) +: rest => 
        var name = ""
        n match {
          case Name(y) => name = y
          case _ => throw new ConversionException("Missing a valid variable name")
        } 
        val start = valToInt(eval(e, env)) 
        val stop = valToInt(eval(e2, env))
        c match {
        case Until =>
          for (x <- start until stop)  {
            val env3 = env + (name -> new Location(Type(Name("Int")), Some(Integer(x))))
            exec(a, env3)
          }
        case To =>
          for (x <- start to stop)  {
            val env3 = env + (name -> new Location(Type(Name("Int")), Some(Integer(x))))
            exec(a, env3)
          }
        }
        exec(rest, env)
      case (e: Expr) +: rest =>
        eval(e, env)
        exec(rest, env)
      case DecVar(t, name, value) +: rest =>
        exec(rest, declare(DecVar(t, name, value), env))
      case DecFunct(t, name, args, body) +: rest =>
        exec(rest, declare(DecFunct(t, name, args, body), env))
      case Reassign(name, value) +: rest =>
        exec(rest, reassign(Reassign(name, value), env))
      case Return(None) +: rest => returnVal = Some(null)
      case Return(Some(expr)) +: rest => 
        exec(List(Print(expr)), env)
        returnVal = Some(eval(expr, env))
      case _ => return //Empty list
      }
    }
    
    def addFunctions(from: Env, to: Env): Env = {
      //Adds all of the functions defined in from to to.
      var result = to
      for((k, v) <- from){
        v.contents match{
          case Some(FunctVal(t, args, body, staticEnv)) => result += (k -> v)
          case _ => null
        }
      }
      result
    }
    
    def execFunct(funct: FunctVal, args: Args, env: Env): Value = {
      //exec the body with the new env.                
      //Arguments have the name of the function's args.
      var newEnv = funct.staticEnv
      for(i <- 0 to funct.args.params.length -1){
        val expr = args.params(i)
        expr match{
          case Name(x) => 
            //Call-by-reference
            //Entry in newEnv points the function definition's arg name to the already stored location.
            newEnv += (funct.args.params(i)._2.name -> env(x)) 
            /*
          case Field(None, name(x)) =>
            //Call-by-reference
            //Note that OOP is not yet implemented.
            newEnv += (funct.args.params(i)._2.name -> env(x))
            */
          case _ =>
            //Call-by-value
            val v = eval(expr, env)
            newEnv += (funct.args.params(i)._2.name -> new Location(funct.args.params(i)._1, Some(v))) 
        }
      }
      
      newEnv = addFunctions(env, newEnv)
      
      exec(funct.body, newEnv)
      
      if(!returnVal.isEmpty){
        val ret = returnVal.get
        returnVal = None
        ret
      }
      else null
    }
    
    exec(program, Map.empty[String, Location])
  }
}
