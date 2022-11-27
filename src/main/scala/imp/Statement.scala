package imp

import com.sun.source.tree.AssignmentTree
import imp.Expr.*

abstract class Statement
case object Skip extends Statement
case object Empty extends Statement
case class Error(msg: String, stmt: Statement) extends Statement
case class PrintStmt(text: String, expr: Expr) extends Statement
case class AssnStmt(lExpr: Var, rExpr: Expr) extends Statement
case class IfStmt(cond: Expr, thenComm: Statement, elseComm: Statement) extends Statement
case class WhileStmt(cond: Expr, body: Statement) extends Statement
case class BlockStmt(sList: List[Statement]) extends Statement {
  def replaceFirst(s: Statement): Statement = {
    s match {
      case Empty => if (sList.isEmpty) Empty else BlockStmt(sList.tail)
      case Skip => if (sList.isEmpty) Empty else BlockStmt(sList.tail)
      case Error(msg, es) => Error(msg, BlockStmt(es :: sList.tail))
      case _ => BlockStmt(s :: sList.tail)
    }
  }
}

object Statement {

  def exec(stmt: Statement): (Statement, State) = exec(stmt, new State())

  def exec(stmt: Statement, state: State): (Statement, State) = {
    stmt match{
      case Skip => (stmt, state)
      case Empty => (stmt, state)
      case Error(msg, e) => (stmt, state)
      case a @ AssnStmt(l, r) => {
        val v = Expr.eval(r, state)
        if(v.isEmpty) (Error("Could not evaluate expression", a), state)
        (stmt, state.updt(l.name, v.get))
      }
      case ifStmt @ IfStmt(cond, thenC, elseC) => {
        val b = Expr.eval(cond, state)
        if (b.isEmpty) (Error("Could not evaluate condition", ifStmt), state)
        b.get match{
          case BoolVal(b) => {
            if(b) exec(thenC, state) else exec(elseC, state)
          }
          case _ => (Error("Condition not a boolean value", ifStmt), state)
        }
      }

      case whileStmt @ WhileStmt(cond, body) => {
        var b = Expr.eval(cond, state)
        var updatedState = state
        if (b.isEmpty || !b.get.isInstanceOf[BoolVal]) (Error("Could not evaluate condition", whileStmt), state)
        while(b.get.asInstanceOf[BoolVal].b){
          val updated = exec(body, updatedState)
          updatedState = updated._2
          b = Expr.eval(cond, updatedState)
          if (b.isEmpty || !b.get.isInstanceOf[BoolVal]) (Error("Could not evaluate condition", whileStmt), state)
        }
        (stmt, updatedState)
      }
      case block @ BlockStmt(sList) => {
        if(!sList.isInstanceOf[List[Statement]] || sList.isEmpty) {
          var i = 1
          return (stmt, state)
        }
        var updated = exec(block.sList.head, state)
        updated._1 match{
          case e @ Error(msg, er) => (e, updated._2)
          case _ => exec(BlockStmt(block.sList.tail), updated._2)
        }

      }
      case PrintStmt(s, e) => {
        System.out.println(s + Expr.eval(e,state).getOrElse(""))
        (stmt, state)
      }
    }}}

