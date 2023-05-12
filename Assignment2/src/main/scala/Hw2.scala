package Hw2

import fastparse._
import MultiLineWhitespace._
import scala.collection.immutable.HashMap 

sealed trait Val
case class IntVal(n: Int) extends Val
case class BoolVal(b: Boolean) extends Val
case class ProcVal(v: Var, expr: Expr, env: Env) extends Val
case class RecProcVal(fv: Var, av: Var, body: Expr, expr: Expr, env: Env) extends Val

case class Env(hashmap: HashMap[Var,Val]) {
  def apply(variable: Var): Val = hashmap(variable)
  def exists(v: Var): Boolean = 
    hashmap.exists((a: (Var, Val)) => a._1 == v)
  def add(v: Var, value: Val) = Env(hashmap + (v -> value))
  
}

sealed trait Program
sealed trait Expr extends Program
case class Const(n: Int) extends Expr
case class Var(s: String) extends Expr
case class Add(l: Expr, r: Expr) extends Expr
case class Sub(l: Expr, r: Expr) extends Expr
case class Iszero(c: Expr) extends Expr
case class Ite(c: Expr, t: Expr, f: Expr) extends Expr
case class Let(name: Var, value: Expr, body: Expr) extends Expr
case class Paren(expr: Expr) extends Expr
case class Proc(v: Var, expr: Expr) extends Expr
case class PCall(ftn: Expr, arg: Expr) extends Expr
case class LetRec(fname: Var, aname: Var, fbody: Expr, ibody: Expr) extends Expr

sealed trait IntExpr
case class IntConst(n: Int) extends IntExpr
case object IntVar extends IntExpr
case class IntAdd(l: IntExpr, r: IntExpr) extends IntExpr
case class IntSub(l: IntExpr, r: IntExpr) extends IntExpr
case class IntMul(l: IntExpr, r: IntExpr) extends IntExpr
case class IntSigma(f: IntExpr, t: IntExpr, b: IntExpr) extends IntExpr
case class IntPow(b: IntExpr, e: IntExpr) extends IntExpr



package object Hw2 {

  

}

object IntInterpreter {
  def evalInt(expr: IntExpr, env: Option[Int]): Int = expr match {
    case IntConst(n) => n
    case IntVar => env match{
      case None => throw new Exception("QOTAQ")
      case Some(x) => x
    }
    case IntAdd(l, r) => (evalInt(l, env), evalInt(r, env)) match {
      case (x: Int, y: Int) => x + y
      case _ => throw new Exception("Type Error")
    }
    case IntSub(l, r) => (evalInt(l, env), evalInt(r, env)) match {
      case (x: Int, y: Int) => x - y
      case _ => throw new Exception("Type Error")
    }
    case IntMul(l, r) => (evalInt(l, env), evalInt(r, env)) match {
      case (x: Int, y: Int) => x*y
      case _ => throw new Exception("Type Error")
    }

    case IntSigma(f, t, b) => (evalInt(f, env), evalInt(t, env), b) match {
      case (x: Int, y: Int, b: IntExpr) =>
        if (x <= y) 
          evalInt(b, Some(x)) + evalInt(IntSigma(IntAdd(IntConst(x), IntConst(1)), IntConst(y), b), env)
        else 0
      case _ => throw new Exception("Type Error")
    }
    case IntPow(b, e) => (evalInt(b, env), evalInt(e, env)) match {
      case (x: Int, y: Int) => 
        if (y == 0)  1
        else{
          x * evalInt(IntPow(IntConst(x), IntConst(evalInt(IntSub(IntConst(y), IntConst(1)), env))), env)
        }
      case _ => throw new Exception("Type Error")
    } 
  }
  def apply(s: String): Int = {
    val parsed = IntParser(s)
    evalInt(parsed, None)
  }
}

object LetRecInterpreter {
  
  def eval(env: Env, expr: Expr): Val = expr match {
    case Const(n) => IntVal(n)
    case Var(s) =>
      if (env.exists(Var(s)))
        env.apply(Var(s))
      else throw new Exception("1")
    case Add(a,b) => (eval(env,a), eval(env,b)) match {
      case (x: IntVal, y: IntVal) => IntVal(x.n + y.n)
      case _ => throw new Exception("Type Error")
    }
    case Sub(a,b) => (eval(env,a), eval(env,b)) match {
      case (x: IntVal, y: IntVal) => IntVal(x.n - y.n)
      case _ => throw new Exception("Type Error")
    }
    case Iszero(c) => eval(env,c) match {
      case (x: IntVal) => BoolVal(x.n == 0)
      case _ => BoolVal(false)
    }
    case Ite(c, t, f) => eval(env,c) match {
      case v: BoolVal => if (v.b) eval(env,t) else eval(env,f)
      case _ => throw new Exception("Type Error")
    }
    case Let(name, value, body) => {
      val new_env = env.add(name, eval(env, value))
      eval(new_env, body)
    }
    case Paren(expr: Expr) => eval(env,expr)
    case Proc(v, expr) => ProcVal(v, expr, env)
    case PCall(ftn, arg) => (eval(env, ftn), eval(env, arg)) match{
      case (RecProcVal(fname, aname, fbody, ibody, env), IntVal(n)) => eval((env.add(aname, IntVal(n))).add(fname, RecProcVal(fname, aname, fbody, ibody, env)), fbody)
      case (ProcVal(v, expr, env), IntVal(n)) => eval(env.add(v, IntVal(n)), expr)
      case _ => throw new Exception("QOTAQTY ZHEME")
    }
    case LetRec(fname, aname, fbody, ibody) => RecProcVal(fname, aname, fbody, ibody, env)
  }
  
  
  def apply(program: String): Val = {
    val parsed = LetRecParserDriver(program)
    eval(Env(new HashMap[Var,Val]()), parsed)
  }

}

object LetRecToString {
  def apply(expr: Expr): String = expr match {
    case Const(n) => n.toString
    case Var(s) => s
    case Add(l, r) => apply(l) + " + " + apply(r)
    case Sub(l, r) => apply(l) + " - " + apply(r)
    case Iszero(x) => "iszero " + apply(x)
    case Ite(c, t, f) => "if " + apply(c) + " then " + apply(t) + " else " + apply(f)
    case Let(name, value, body) => "let " + apply(name) + " = " + apply(value) + " in " + apply(body)
    case Paren(expr) => "(" + apply(expr) + ")"
    case Proc(v, expr) => "proc " + apply(v) + " " + apply(expr)
    case PCall(ftn, arg) => apply(ftn) + " " + apply(arg)
    case LetRec(fname, aname, fbody, ibody) => "letrec " + apply(fname) + apply(Paren(aname)) + " = " + apply(fbody) + " in " + apply(ibody)
  }
}

object Hw2App extends App {
  
  println("Hello from Hw2!")

  val int_prog = """x + 1"""
  val parsed = IntParser(int_prog)
  println(parsed)


}