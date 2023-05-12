package hw3

import scala.collection.immutable.HashMap 
import hw3._


package object hw3 {
  type Env = HashMap[Var,Val]
  type Loc = Int
  
}

case class Mem(m: HashMap[Loc,Val], top: Loc) {
  def apply(l: Loc): Val = m(l)
  def exists(l: Loc): Boolean = m.exists((a: (Loc, Val)) => a._1 == l)
  def add(l: Loc, value: Val) = Mem(m + (l -> value), l + 1)
}

sealed trait Val
case class IntVal(n: Int) extends Val
case class BoolVal(b: Boolean) extends Val
case class ProcVal(v: Var, expr: Expr, env: Env) extends Val
case class RecProcVal(fv: Var, av: Var, body: Expr, env: Env) extends Val
case class LocVal(l: Loc) extends Val


sealed trait Program
sealed trait Expr extends Program
case class Const(n: Int) extends Expr
case class Var(s: String) extends Expr
case class Add(l: Expr, r: Expr) extends Expr
case class Sub(l: Expr, r: Expr) extends Expr
case class Mul(l: Expr, r: Expr) extends Expr
case class Div(l: Expr, r: Expr) extends Expr
case class GTExpr(l: Expr, r: Expr) extends Expr
case class GEQExpr(l: Expr, r: Expr) extends Expr
case class Iszero(c: Expr) extends Expr
case class Ite(c: Expr, t: Expr, f: Expr) extends Expr
case class ValExpr(name: Var, value: Expr, body: Expr) extends Expr
case class VarExpr(name: Var, value: Expr, body: Expr) extends Expr
case class Proc(v: Var, expr: Expr) extends Expr
case class DefExpr(fname: Var, aname: Var, fbody: Expr, ibody: Expr) extends Expr
case class Asn(v: Var, e: Expr) extends Expr
case class Paren(expr: Expr) extends Expr
case class Block(f: Expr, s: Expr) extends Expr
case class PCall(ftn: Expr, arg: Expr) extends Expr







object MiniScalaInterpreter {

  case class Result(v: Val, m: Mem)
  case class UndefinedSemantics(msg: String = "", cause: Throwable = None.orNull) extends Exception("Undefined Semantics: " ++ msg, cause)


  def doInterpret(env: Env, mem: Mem, expr: Expr): Result = expr match {
    case Const(n) => Result(IntVal(n), mem) 
    case Var(s) => {
      // println("----QAZAQ-----")
      // println(env)
      if (env.exists((a: (Var, Val)) => a._1 == Var(s))) env(Var(s)) match {
          case LocVal(l) => 
            if (mem.exists(l)) {
              Result(mem.apply(l), mem)
            }
            else throw new UndefinedSemantics(s"Var Exception")
          case _ => Result(env(Var(s)), mem)
      }
      else {
        println(s)
        throw new UndefinedSemantics("Undefined for Var")
      }
    }
    case Add(l, r) => (doInterpret(env, mem, l).v, doInterpret(env, doInterpret(env, mem, l).m, r).v) match {
      case (x: IntVal, y: IntVal) => Result(IntVal(x.n + y.n), mem)
      case _ => throw new UndefinedSemantics(s"Wrong Type 1")
    }
    case Sub(l, r) => (doInterpret(env, mem, l).v, doInterpret(env, doInterpret(env, mem, l).m, r).v) match {
      case (x: IntVal, y: IntVal) => Result(IntVal(x.n - y.n), mem)
      case _ => throw new UndefinedSemantics(s"Wrong Type 2")
    }
    case Mul(l, r) => (doInterpret(env, mem, l).v, doInterpret(env, doInterpret(env, mem, l).m, r).v) match {
      case (x: IntVal, y: IntVal) => Result(IntVal(x.n * y.n), mem)
      case _ => throw new UndefinedSemantics(s"Wrong Type 3")
    }    
    case Div(l, r) => (doInterpret(env, mem, l).v, doInterpret(env, doInterpret(env, mem, l).m, r).v) match {
      case (x: IntVal, y: IntVal) => 
        if (y.n != 0) Result(IntVal(x.n / y.n), mem) else throw new UndefinedSemantics(s"Wrong type")
      case _ => throw new UndefinedSemantics(s"Wrong Type 4")
    }
    case GTExpr(l, r) => (doInterpret(env, mem, l).v, doInterpret(env, doInterpret(env, mem, l).m, r).v) match {
      case (x: IntVal, y: IntVal) => if (x.n > y.n) Result(BoolVal(true), mem) else Result(BoolVal(false), mem)
      case _ => throw new UndefinedSemantics(s"Wrong Type 5")
    }
    case GEQExpr(l, r) => (doInterpret(env, mem, l).v, doInterpret(env, doInterpret(env, mem, l).m, r).v) match {
      case (x: IntVal, y: IntVal) => if (x.n >= y.n) Result(BoolVal(true), mem) else Result(BoolVal(false), mem)
      case _ => throw new UndefinedSemantics(s"Wrong Type 6")
    }
    case Iszero(c) => (doInterpret(env, mem, c).v) match {
      case (x: IntVal) => if (x.n == 0) Result(BoolVal(true), doInterpret(env, mem, c).m) else Result(BoolVal(false), doInterpret(env, mem, c).m)
      case _ => throw new UndefinedSemantics(s"Wrong Type 7")
    }
    case Ite(c, t, f) => doInterpret(env, mem, c).v match {
      case (v: BoolVal) => if (v.b) doInterpret(env, doInterpret(env, mem, c).m, t) else doInterpret(env, doInterpret(env, mem, c).m, f)
      case _ => throw new UndefinedSemantics(s"Wrong Type 8")
    }
    case ValExpr(name, value, body) => {
      val new_result = doInterpret(env, mem, value)
      val new_env = env + (name -> new_result.v)
      doInterpret(new_env, new_result.m, body)
    } 
    case VarExpr(name, value, body) => {
      val new_result = doInterpret(env, mem, value)
      val new_top = (new_result.m).top
      val new_env = env + (name -> LocVal(new_top))
      val new_mem = (new_result.m).add(new_top, new_result.v)
      doInterpret(new_env, new_mem, body)
    }
    case Proc(v, expr) => Result(ProcVal(v, expr, env), mem)

    case Paren(expr) => doInterpret(env, mem, expr)

    case DefExpr(fname, aname, fbody, ibody) => {
      val new_env = env + (fname -> RecProcVal(fname, aname, fbody, env))
      doInterpret(new_env, mem, ibody)
    }
    case Asn(v, e) => {
      val new_result = doInterpret(env, mem, e)

      if (env.exists((a: (Var, Val)) => a._1 == v)){
        env(v) match {
          case LocVal(location) => {
            val new_mem = (new_result.m).add(location, new_result.v)
            Result(new_result.v, new_mem)
          }
          case _ => throw new UndefinedSemantics(s"Wrong type 8")
        }
      }
      else throw new UndefinedSemantics(s"Wrong type 9")
    }
    case Block(f, s) => {
      val new_result = doInterpret(env, mem, f)
      doInterpret(env, new_result.m, s)
    }
    case PCall(ftn, arg) => 
      val result = doInterpret(env, mem, ftn)
      result.v match {
        case ProcVal(value, expr, env1) =>{
          val new_result = doInterpret(env, result.m, arg)

          val new_env = env1 + (value -> new_result.v)
          doInterpret(new_env, new_result.m, expr)
        }
      
        case RecProcVal(fv, av, body, env1) => {
          val new_result = doInterpret(env, result.m, arg)
          
          val new_env = env1 + (av -> new_result.v) + (fv -> RecProcVal(fv, av, body, env1))

          doInterpret(new_env, new_result.m, body)
        }
        case _ => throw new UndefinedSemantics(s"Wrong type 10")
      }
  }

  def apply(program: String): Val = {
    val parsed = MiniScalaParserDriver(program)

    doInterpret(new Env(), Mem(new HashMap[Loc,Val],0), parsed).v
  }

}


object Hw3App extends App {

  println("Hello from Hw3!")

}
