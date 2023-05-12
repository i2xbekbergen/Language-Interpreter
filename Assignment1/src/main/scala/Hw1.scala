// Name: Aibek Bekbergen
// Student ID: 20202012
sealed trait IntList
case object Nil extends IntList
case class Cons(v: Int, t: IntList) extends IntList

sealed trait BTree
case object Leaf extends BTree
case class IntNode(v: Int, left: BTree, right: BTree)
extends BTree

sealed trait Formula
case object True extends Formula
case object False extends Formula
case class Not(f: Formula) extends Formula
case class Andalso(left: Formula, right: Formula) extends Formula
case class Orelse(left: Formula, right: Formula)  extends Formula
case class Implies(left: Formula, right: Formula) extends Formula

object Hw1 extends App {

  println("Hw1!")

  def fibo(n: Int): Int = {
    if (n == 0) {
      return 1;
    } else if (n == 1){
      return 1;
    } else {
      return fibo(n-1) + fibo(n-2);
    }
  }

  
  def sum(f: Int=>Int, n: Int): Int = {
    if (n <= 1){
      return n;
    }
    else{
      return f(n)+sum(f, n-1);
    }
  }

  
  def foldRight(init: Int, ftn: (Int, Int)=>Int, list: IntList): Int = {
    list match{
      case Nil => init
      case Cons(h,t) => ftn(foldRight(init, ftn, t), h)
    }
  }
  
  def filter(f: Int => Boolean, list: IntList): IntList = {
    list match {
      case Nil => Nil
      case Cons(h, t) => if (f(h)) Cons(h, filter(f, t)) else filter(f, t)
    }
  }

  def iter[A](f: A => A, n: Int): A => A = (x : A) => {

    // I found the following function on StackOverflow:
    //      Function.chain(List.fill(n)(f))
    // 
    // link: https://stackoverflow.com/questions/46497079/scala-generics-with-recursion
    // 
    //
    // However, I am not sure if we are allowed to used it. 
    // that is why I tried to solve it the other way
    // 
    if(n == 0){
      x
    } else{
      iter(f, n-1)(f(x))
    }
  }

  def insert(t: BTree, a: Int): BTree = {
    t match{
      case Leaf => IntNode(a, Leaf, Leaf)
      case IntNode(v, left, right) => 
        if (a < v){
          IntNode(v, insert(left, a), right)
        }
        else{
          IntNode(v, left, insert(right, a))
        }
    }
  }

  def eval(f: Formula): Boolean = {
    f match{
      case True => true
      case False => false
      case Not(x) => 
        if (x == True) false
        else true
      case Andalso(left, right) => (eval(left) && eval(right))
        // if ((eval(left) == true) && (eval(right) == true)) true
        // else false
      case Orelse(left, right) => (eval(left) || eval(right))
        // if ((eval(left) == false)&&(eval(right) == false)) false
        // else true
      case Implies(left, right) => 
        if ((eval(left) == true)&&(eval(right) == false)) false
        else true
    }
  }

}