package hwk4
import scala.collection.mutable
import scala.collection.mutable.Set
import scala.collection.mutable.Queue
import scala.collection.mutable.Map
import hwk.{ArrayLit, AssignExpr, BracketRef, CondExpr, DotRef, Expression, FuncCall, InfixExpr,
  MethodCall, NewCall, ObjectLit, PrefixExpr, Script, Statement, VarRef,ListExpr,BlockStmt,VarDeclListStmt,
  VarDeclStmt,ExprStmt,IfStmt,WhileStmt,LVarRef,DoWhileStmt,SwitchStmt,CaseStmt}

//import common._

object Util {
  def aexp(e: Expression): Set[Expression] =
    e match {
      case DotRef(o,_) => aexp(o)
      case BracketRef(o,_) => aexp(o)
      case MethodCall(r,m,a) => aexp(r).union(aexp(m)).union(aexp(a))
      case FuncCall(f,a) => aexp(f).union(aexp(a))
      case NewCall(f,a) => aexp(f).union(aexp(a))
      case AssignExpr(_, _, e) => aexp(e) + e // not quite right
      case ObjectLit(obj) => aexp(obj.map(o => o.expr))
      case ArrayLit(vs) => aexp(vs)
      case PrefixExpr(_, e) => aexp(e)
      case InfixExpr(_, e1, e2) => aexp(e1).union(aexp(e2)) + e
      case CondExpr(cond, thenPart, elsePart) => aexp(List(cond,thenPart,elsePart))
      case ListExpr(es) => aexp(es)
      case _ => Set()
    }
  def aexp(es: List[Expression]): Set[Expression] = es.map(e=>aexp(e)).foldLeft(Set[Expression]())((a,b)=>a.union(b))

  def aexp(stmt: Statement): Set[Expression] = {
    stmt match {
      case Script(stmts) => aexps(stmts)
      case BlockStmt(stmts) => aexps(stmts)
      case VarDeclListStmt(stmts) => aexps(stmts)
      case VarDeclStmt(x, e) => aexp(e)
      case ExprStmt(AssignExpr(_, _, e)) => aexp(e)
      case IfStmt(c, t, e) => aexp(c).union(aexp(t)).union(aexp(e))
      case WhileStmt(c, b) => aexp(c).union(aexp(b))
      case DoWhileStmt(c, b) => aexp(c).union(aexp(b))
      case SwitchStmt(e, cases, d) => aexp(e).union(aexps(d match { case Some(c) => c::cases case None => cases }))
      case CaseStmt(e, s) => aexp(e).union(aexp(s))
      case _ => Set()
    }
  }
  def aexps(stmts: List[Statement]): Set[Expression] = stmts.map(s => aexp(s)).foldLeft(Set[Expression]())((a,b)=>a.union(b))

  def fv(e: Expression): Set[String] =
    e match {
      case VarRef(n) => Set(n)
      case DotRef(o,_) => fv(o)
      case BracketRef(o,_) => fv(o)
      case MethodCall(r,m,a) => fv(r).union(fv(m)).union(fv(a))
      case FuncCall(f,a) => fv(f).union(fv(a))
      case NewCall(f,a) => fv(f).union(fv(a))
      case AssignExpr(_, lv, e) => fv(e) // not quite right
      case ObjectLit(obj) => fv(obj.map(o => o.expr))
      case ArrayLit(vs) => fv(vs)
      case PrefixExpr(_, e) => fv(e)
      case InfixExpr(_, e1, e2) => fv(e1).union(fv(e2))
      case CondExpr(cond, thenPart, elsePart) => fv(List(cond,thenPart,elsePart))
      case ListExpr(es) => fv(es)
      case _ => Set()
    }
  def fv(es: List[Expression]): Set[String] = es.map(e=>fv(e)).foldLeft(Set[String]())((a,b)=>a.union(b))

  def vars(stmt: Statement): Set[String] = {
    stmt match {
      case Script(stmts) => vars(stmts)
      case BlockStmt(stmts) => vars(stmts)
      case VarDeclListStmt(stmts) => vars(stmts)
      case VarDeclStmt(x, e) => Set(x.str)
      case ExprStmt(AssignExpr(_, LVarRef(n), _)) => Set(n)
      case IfStmt(_, t, e) => vars(t).union(vars(e))
      case WhileStmt(_, b) => vars(b)
      case DoWhileStmt(_, b) => vars (b)
      case SwitchStmt(_, cases, d) => vars(d match { case Some(c) => c::cases case None => cases })
      case CaseStmt(_, s) => vars(s)
      case _ => Set()
    }
  }
  def vars(stmts: List[Statement]): Set[String] = stmts.map(s => vars(s)).foldLeft(Set[String]())((a,b)=>a.union(b))

}