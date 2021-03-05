package hwk4


import hwk.{AssignExpr, EmptyExpr, ExprStmt, Expression, IfStmt, IntroduceVar, LVarRef, Statement, VarDeclStmt, WhileStmt}
import hwk4.{Analysis, CFG, ForwardCFG, Lattice, Util}

import scala.collection.mutable.Set

case class Aex(exps: Set[Expression]) extends Lattice[Aex]{
  def lub(that: Aex)= Aex(exps.intersect(that.exps))

  override def toString: String = "{"+exps.toList.sortBy(x=>x.toString).mkString(",")+"}"
}
case class RD (stmt: Statement) extends Analysis[Aex]{
  val cfg: CFG = ForwardCFG(stmt)
  val extremalValue = Aex(Set())
  val bottom = Aex(Util.aexp(stmt))
  val entry= real_entry
  val exit= real_exit

  def transfer(stmt: Statement, ell: Aex) ={
    def f(y: String, e:Expression) ={
      val s= ell.exps.filter({case x => Util.fv(x).contains(y)})

      Aex( s)
    }
    def gen(e:Expression)= Aex(ell.exps.union(Util.aexp(e)))

    stmt match{
      case VarDeclStmt(IntroduceVar(y), e)=>{
        e match {
          case EmptyExpr()=> ell
          case _ => f(y,e)
        }
      }
      case ExprStmt(AssignExpr(_, LVarRef(y),e )) => f(y,e)
      case ExprStmt(e)=> gen(e)
      case WhileStmt(a,_)=> gen(a)
      case IfStmt(e,_,_)=> gen(e)
      case _=> ell
    }

  }
}