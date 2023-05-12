package hw4
// The following functions were taken from the Assignment 3:
// Add, Sub, Mul, Div, Iszero, Ite, Proc, Asn, Block
import scala.collection.immutable.HashMap 
import hw4._


package object hw4 {
  type Env = HashMap[Var,LocVal]
}

case class Mem(m: HashMap[LocVal,Val], top: Int) {
  def extended(v: Val): (Mem, LocVal) = {
    val new_mem = Mem(m.updated(LocVal(top),v), top+1)
    (new_mem,LocVal(top))
  }
  def updated(l: LocVal, new_val: Val): Option[Mem] = {
    m.get(l) match {
      case Some(v) => Some(Mem(m.updated(l, new_val), top))
      case None => None
    }
  }
  def get(l: LocVal): Option[Val] = m.get(l)
  def getLocs(): List[LocVal] = m.keySet.toList
  // add(l, value) and exists(l) functions were added from the assignment 3 for the convenience
  def add(l: LocVal, value: Val) = Mem(m + (l -> value), l.l + 1)
  def exists(l: LocVal): Boolean = m.exists((a: (LocVal, Val)) => a._1 == l)
}

sealed trait Val
case object SkipVal extends Val
case class IntVal(n: Int) extends Val
case class BoolVal(b: Boolean) extends Val
case class ProcVal(args: List[Var], expr: Expr, env: Env) extends Val
case class LocVal(l: Int) extends Val
sealed trait RecordValLike extends Val
case object EmptyRecordVal extends RecordValLike
case class RecordVal(field: Var, loc: LocVal, next: RecordValLike) extends RecordValLike


sealed trait Program
sealed trait Expr extends Program
case object Skip extends Expr
case object False extends Expr
case object True extends Expr
case class NotExpr(expr: Expr) extends Expr
case class Const(n: Int) extends Expr
case class Var(s: String) extends Expr {
  override def toString = s"Var(${"\""}${s}${"\""})"
}
case class Add(l: Expr, r: Expr) extends Expr
case class Sub(l: Expr, r: Expr) extends Expr
case class Mul(l: Expr, r: Expr) extends Expr
case class Div(l: Expr, r: Expr) extends Expr
case class LTEExpr(l: Expr, r: Expr) extends Expr
case class EQExpr(l: Expr, r: Expr) extends Expr
case class Iszero(c: Expr) extends Expr
case class Ite(c: Expr, t: Expr, f: Expr) extends Expr
case class Let(i: Var, v: Expr, body: Expr) extends Expr
case class Proc(args: List[Var], expr: Expr) extends Expr
case class Asn(v: Var, e: Expr) extends Expr
case class BeginEnd(expr: Expr) extends Expr
case class FieldAccess(record: Expr, field: Var) extends Expr
case class FieldAssign(record: Expr, field: Var, new_val: Expr) extends Expr
case class Block(f: Expr, s: Expr) extends Expr
case class PCallV(ftn: Expr, arg: List[Expr]) extends Expr
case class PCallR(ftn: Expr, arg: List[Var]) extends Expr
case class WhileExpr(cond: Expr, body: Expr) extends Expr
sealed trait RecordLike extends Expr
case object EmptyRecordExpr extends RecordLike
case class RecordExpr(field: Var, initVal: Expr, next: RecordLike) extends RecordLike








object MiniCInterpreter {

  case class Result(v: Val, m: Mem)
  case class New_Result(e: Env, m: Mem)
  case class UndefinedSemantics(msg: String = "", cause: Throwable = None.orNull) extends Exception("Undefined Semantics: " ++ msg, cause)
  
  def eval(env: Env, mem: Mem, expr: Expr): Result = expr match  {
    case Skip => Result(SkipVal, mem)

    case False => Result(BoolVal(false), mem)

    case True => Result(BoolVal(true), mem)

    case NotExpr(expr) => eval(env, mem, expr).v match {
      case BoolVal(b) =>
        if (b == true) Result(BoolVal(false), mem)
        else Result(BoolVal(true), mem)
      case _ => throw new UndefinedSemantics(s"NotExpr")
    } 

    case Const(n) => Result(IntVal(n), mem)
    
    case Var(s) => 
      if (env.exists((a: (Var, LocVal)) => a._1 == Var(s))){
        env(Var(s)) match {
          case LocVal(l) => 
            if(mem.exists(LocVal(l))) Result(mem.m(LocVal(l)), mem) else throw new UndefinedSemantics(s"No Var in Memory")
          case _ => throw new UndefinedSemantics(s"Exception!")
        }
      } else throw new UndefinedSemantics(s"Var Exception")
    
    case Add(l, r) => (eval(env, mem, l).v, eval(env, eval(env, mem, l).m, r).v) match {
      case (x: IntVal, y: IntVal) => Result(IntVal(x.n + y.n), mem)
      case _ => throw new UndefinedSemantics(s"Wrong Type 1")
    }
    case Sub(l, r) => (eval(env, mem, l).v, eval(env, eval(env, mem, l).m, r).v) match {
      case (x: IntVal, y: IntVal) => Result(IntVal(x.n - y.n), mem)
      case _ => throw new UndefinedSemantics(s"Wrong Type 2")
    }
    case Mul(l, r) => (eval(env, mem, l).v, eval(env, eval(env, mem, l).m, r).v) match {
      case (x: IntVal, y: IntVal) => Result(IntVal(x.n * y.n), mem)
      case _ => throw new UndefinedSemantics(s"Wrong Type 3")
    }    
    case Div(l, r) => (eval(env, mem, l).v, eval(env, eval(env, mem, l).m, r).v) match {
      case (x: IntVal, y: IntVal) => 
        if (y.n != 0) Result(IntVal(x.n / y.n), mem) else throw new UndefinedSemantics(s"Wrong type")
      case _ => throw new UndefinedSemantics(s"Wrong Type 4")
    }
    
    case LTEExpr(l, r) => (eval(env, mem, l).v, eval(env, eval(env, mem, l).m, r).v) match {
      case (x: IntVal, y: IntVal) => if (x.n <= y.n) Result(BoolVal(true), mem) else Result(BoolVal(false), mem)
      case _ => throw new UndefinedSemantics(s"Wrong Type 5")
    }
    case EQExpr(l, r) => eval(env, mem, l).v match{
      case (x: IntVal) => eval(env, eval(env, mem, l).m, r).v match {
        case (y: IntVal) => Result(BoolVal(x.n == y.n), eval(env, eval(env, mem, l).m, r).m)
        case _ => Result(BoolVal(false), eval(env, mem, l).m)
      }
      case (x: BoolVal) => eval(env, eval(env, mem, l).m, r).v match {
        case (y: BoolVal) => Result(BoolVal(x.b == y.b), eval(env, eval(env, mem, l).m, r).m)
        case _ => Result(BoolVal(false), eval(env, mem, l).m)
      }
      case (SkipVal) => eval(env, eval(env, mem, l).m, r).v match {
        case (SkipVal) => Result(BoolVal(true), eval(env, eval(env, mem, l).m, r).m)
        case _ => Result(BoolVal(false), eval(env, mem, l).m)
      }
      case _ => throw new UndefinedSemantics(s"Exception!")
    }

    case Iszero(c) => eval(env, mem, c).v match {
      case (x: IntVal) => if (x.n == 0) Result(BoolVal(true), eval(env, mem, c).m) else Result(BoolVal(false), eval(env, mem, c).m)
      case _ => throw new UndefinedSemantics(s"Iszero Exception")
    }

    case Ite(c, t, f) => eval(env, mem, c).v match {
      case (v: BoolVal) => if (v.b) eval(env, eval(env, mem, c).m, t) else eval(env, eval(env, mem, c).m, f)
      case _ => throw new UndefinedSemantics(s"Wrong Type 8")
    }

    case Let(i, v, body) => 
      val new_result = eval(env, mem, v)
      val new_mem = new_result.m.extended(new_result.v)
      val new_env = env + (i -> new_mem._2)
      eval(new_env, new_mem._1, body)

    case Proc(args, expr) => Result(ProcVal(args, expr, env), mem)
    
    case Asn(v, e) => {
      val new_result = eval(env, mem, e)
      if (env.exists((a: (Var, Val)) => a._1 == v)){
        env(v) match {
          case LocVal(location) => {
            val new_mem = (new_result.m).add(LocVal(location), new_result.v)
            Result(new_result.v, new_mem)
          }
          case _ => throw new UndefinedSemantics(s"Wrong type 8")
        }
      } else throw new UndefinedSemantics(s"Wrong type 9")
    }

    case BeginEnd(expr) => eval(env, mem, expr)

    case Block(f, s) => eval(env, eval(env, mem, f).m, s)

    case EmptyRecordExpr => Result(EmptyRecordVal, mem)

    case WhileExpr(cond, body) => eval(env, mem, cond).v match{
      case BoolVal(b) => 
        if (b == false) Result(SkipVal, eval(env, mem, cond).m)
        else eval(env, eval(env, eval(env, mem, cond).m, body).m, WhileExpr(cond, body))
      case _ => throw new UndefinedSemantics(s"WhileExpr Exception")
    }

    case PCallR(ftn, arg) => eval(env, mem, ftn).v match {
        case (ProcVal(args, expr, env1)) => {
          if(args.length != arg.length) throw new UndefinedSemantics(s"PCallR Length Exception!")
          else {
            def extend_env(cur: Int, nenv: Env): Env = {              
              if(args.lift(cur) != None){
                val new_res = extend_env(cur + 1, nenv)
                if(env.exists((a: (Var, LocVal)) => a._1 == arg(cur))){
                  val new_env = new_res + (args(cur) -> env(arg(cur)))
                  new_env
                } else throw new UndefinedSemantics(s"PCallR Not in Env Exception!")
              } else nenv
            }
            val new_res = extend_env(0, env1)
            eval(new_res, eval(env, mem, ftn).m, expr)
          }
        }
        case _ => throw new UndefinedSemantics(s"Exception!")
    }
    

    case PCallV(ftn, arg) => eval(env, mem, ftn).v match {
        case (ProcVal(args, expr, env1)) => 
          if(args.length != arg.length) throw new UndefinedSemantics(s"PCallV Length Exception!")
          else {
            def extend_mem(cur: Int, new_mem: Mem): (List[Val], Mem) = {
              if(cur < arg.length){
                val new_res = eval(env, new_mem, arg(cur))
                val returned = extend_mem(cur + 1, new_res.m)
                (List[Val](new_res.v) ::: returned._1, returned._2)
              } else (Nil, new_mem)
            }
            val list =  extend_mem(0, eval(env, mem, ftn).m)._1
            def extend_env(cur: Int, enve: Env, new_mem: Mem): New_Result = {
              if(cur < args.length){
                val new_extended_env = extend_env(cur + 1, enve, new_mem).e + (args(cur) -> LocVal(extend_env(cur + 1, enve, new_mem).m.top))
                val new_extended_mem = extend_env(cur + 1, enve, new_mem).m.add(LocVal(extend_env(cur + 1, enve, new_mem).m.top), list(cur))
                New_Result(new_extended_env, new_extended_mem)
              } else New_Result(enve, new_mem)
            }
            val new_result = extend_env(0, env1, extend_mem(0, eval(env, mem, ftn).m)._2)
            eval(new_result.e, new_result.m, expr)
          }
        case _ => throw new UndefinedSemantics(s"${eval(env, mem, ftn).v} is not a ProcVal!");
    }
    

    case FieldAccess(record, field) => eval(env, mem, record).v match {
        case (r: RecordVal) => Result(eval(env, mem, record).m.m(New_Field(r, field)), eval(env, mem, record).m)
        case _ => throw new UndefinedSemantics(s"Exception!")
    }


    case FieldAssign(record, field, value) => eval(env, mem, record).v match{
      case (r: RecordVal) => 
        val new_result = eval(env, eval(env, mem, record).m, value)
        val new_mem = new_result.m.updated(New_Field(r, field), new_result.v)
        new_mem match {
          case Some(t) => Result(new_result.v, t)
          case None => throw new UndefinedSemantics(s"Exception!")
        }
      case _ => throw new UndefinedSemantics(s"Exception!")
    }

    case RecordExpr(field, initVal, next) => 
      eval(env, mem, initVal) match {
        case Result(r: Val, mem1) =>      
          eval(env, mem1, next).v match {
            case nxt : RecordValLike => Result(RecordVal(field, eval(env, mem1, next).m.extended(r)._2, nxt), eval(env, mem1, next).m.extended(r)._1)
            case _ => throw new UndefinedSemantics(s"[Record`Expr 2]")
          }
        case _ => throw new UndefinedSemantics(s"[Record`Expr]")
      }
  }

  def New_Field(new_record: RecordValLike, new_field: Var): LocVal = new_record match {
    case (RecordVal(field, loc, next)) => 
      if(field == new_field) loc
      else New_Field(next, new_field)
    case _ => throw new UndefinedSemantics(s"Exception!")
  }


  def gc(env: Env, mem: Mem): Mem = {
    if (!env.isEmpty) {
      mem.get(env.head._2) match {
        case Some(l: LocVal) => {
          val new_mem = gc(env-env.head._1 + (env.head._1 -> l), mem)
          Mem(new_mem.m + (env.head._2-> l), gc(env-env.head._1, mem).top)
        }  
        case Some(record: RecordValLike) => record match {
          case RecordVal(field, loc, next) => {
            val new_mem = gc(env-env.head._1 + (env.head._1 -> loc), mem)
            Mem(new_mem.m + (env.head._2 -> record), gc(env-env.head._1, mem).top)}
          case _ => gc(env-env.head._1, mem)
        }
        case Some(smth) => Mem(gc(env-env.head._1, mem).m + (env.head._2-> smth), gc(env-env.head._1, mem).top) 
        case None => gc(env-env.head._1, mem)
      }
    }
    else Mem(new HashMap[LocVal,Val], mem.top)
  }
  
  def apply(program: String): (Val, Mem) = {
    val parsed = MiniCParserDriver(program)
    val res = eval(new Env(), Mem(new HashMap[LocVal,Val],0), parsed)
    (res.v, res.m)
  }

}


object Hw4App extends App {
  
  println("Hello from Hw4!")

}