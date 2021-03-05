//package hwk4
//
//import hwk.{AssignExpr, Expression, IntroduceVar, Statement,ExprStmt,VarDeclStmt,EmptyExpr,LVarRef,
//  WhileStmt,IfStmt}
//
//import scala.collection.mutable
//import scala.collection.mutable.Set
//
//case class VB (ast: Statement) extends Analysis[Aex]{
//  override val cfg: CFG = BackwardCFG(ast)
//  override val extremalValue = Aex(Set())
//  override val bottom = Aex(Set(ast))
//  override val entry: mutable.Map[Node, Aex] = real_exit
//  override val exit: mutable.Map[Node, Aex] = real_entry
//
//  def transfer(stmt: Statement, ell: Aex): Aex ={
//    def f(y: String, e:Expression): Unit ={
//      val s= ell.exps.filter({case e => Util.fv(e).contains(y)})
//
//      Aex(s)
//    }
//    def gen(e:Expression)= Aex(ell.exps.union(Util.aexp(e)))
//
//    ast match{
//      case VarDeclStmt(IntroduceVar(y), e)=>{
//        e match {
//          case EmptyExpr()=> ell
//          case _ => f(y,e)
//        }
//      }
//      case ExprStmt(AssignExpr(_, LVarRef(y),e ))=> f(y,e)
//      case ExprStmt(e)=> gen(e)
//      case WhileStmt(a,_)=> gen(a)
//      case IfStmt(e,_,_)=> gen(e)
//      case _=> ell
//    }
//
//  }
//}