package hwk4


import hwk.{AssignExpr, EmptyExpr, ExprStmt, Expression, IfStmt, IntroduceVar, LVarRef, Statement, VarDeclStmt, WhileStmt}
import hwk4.{Analysis, CFG, ForwardCFG, Lattice, Util}

import scala.collection.mutable.Set

case class Alv(exps: Set[Expression]) extends Lattice[Alv]{
  def lub(that: Alv)= Alv(exps.intersect(that.exps))

  override def toString: String = "{"+exps.toList.sortBy(x=>x.toString).mkString(",")+"}"
}
case class LV (stmt: Statement) extends Analysis[Alv]{
  val cfg: CFG = BackwardCFG(stmt)
  val extremalValue = Alv(Set())
  val bottom = Alv(Util.aexp(stmt))
  val entry= real_exit
  val exit= real_entry

  def transfer(stmt: Statement, ell: Alv) ={
    def f(y: String, e:Expression) ={
      val s= ell.exps.filter({case x => ! Util.fv(x).contains(y)})

      Alv(s)
    }
    def gen(e:Expression)= Alv(ell.exps.union(Util.aexp(e)))

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