package imp

abstract class Expr
abstract class Val(val x: AnyVal) extends Expr
case class IntVal(i: Int) extends Val(i)
case class BoolVal(b: Boolean) extends Val(b)
case class Var(name: String) extends Expr
case class UnyExpr(op: String, expr: Expr) extends Expr
case class BinExpr(op: String, left: Expr, right: Expr) extends Expr
case class ReadInt(prompt: String) extends Expr
case class ReadBool(prompt: String) extends Expr

object Expr {
  private val intOp = List("+", "-", "*", "/", "%")
  private val relOp = List("<", ">", ">=", "<=")
  private val eqOp = List("==", "!=")
  private val boolOp = List("&&", "||")

  def eval(expr: Expr, state: State): Option[Val] = {
    expr match {
      case ReadBool(prompt) => evalReadBool(prompt)
      case ReadInt(prompt) => evalReadInt(prompt)
      case v @ IntVal(i) => Some(v)
      case v @ BoolVal(b) => Some(v)
      case Var(v) => evalVar(v, state)
      case UnyExpr(op, expr) => evalUnyExpr(op, expr, state)
      case BinExpr(op, left, right) => evalBinExpr(op, left, right, state)
      case _ => None
  }
  }

  private def evalBinExpr(operator: String, left: Expr, right: Expr, state: State): Option[Val] = {
    val leftVal = eval(left, state)
    val rightVal = eval(right,state)
    if(leftVal.isEmpty || rightVal.isEmpty) None else{
      leftVal.get match{
        case leftB @ IntVal(i) => { 
          rightVal.get match{
            case rightB @ IntVal(i) => {
              operator match{
                case "+" => Some(IntVal(leftB.i + rightB.i))
                case "-" => Some(IntVal(leftB.i - rightB.i))
                case "*" => Some(IntVal(leftB.i * rightB.i))
                case "%" => Some(IntVal(leftB.i % rightB.i))
                case "/" => Some(IntVal(leftB.i / rightB.i))
                case "<" => Some(BoolVal(leftB.i < rightB.i))
                case ">" => Some(BoolVal(leftB.i > rightB.i))
                case ">=" => Some(BoolVal(leftB.i >= rightB.i))
                case "<=" => Some(BoolVal(leftB.i <= rightB.i))
                case "==" => Some(BoolVal(leftB.i == rightB.i))
                case "!=" => Some(BoolVal(leftB.i != rightB.i))
                case _ => None
              }
            }
            case BoolVal(i) => None
            case _ => None
          }
        }
        case leftB @ BoolVal(b) => {
          rightVal.get match{
            case rightB @ BoolVal(i) => {
              operator match{
                case "!=" => Some(BoolVal(leftB.b != rightB.b))
                case "==" => Some(BoolVal(leftB.b == rightB.b))
                case "&&" => Some(BoolVal(leftB.b && rightB.b))
                case "||" => Some(BoolVal(leftB.b || rightB.b))
                case _ => None
              }
            }
            case IntVal(i) => None
            case _ => None
          }
        }
        case _ => None
      }
    }
  }

  private def evalVar(name: String, state: State): Option[Val] = {
    val option = state.read(name)
    option match{
      case None => None
      case _ => if (option.isEmpty) None else Some(option.get)
    }
  }

  private def evalUnyExpr(operator: String, expr: Expr, state: State): Option[Val] = {
   val option = eval(expr, state)
   option match{
      case None => None
      case _ => {
        if (option.isEmpty) None else {
          evalUnyExprVal(operator, option.get, state)
        }
      }

    }
  }

  private def evalUnyExprVal(operator: String, v: Val, state: State): Option[Val] = {
    v match{
      case IntVal(i) => {
        operator match{
          case "-" => Some(IntVal(-i))
          case _ => None
        }
      }
      case BoolVal(b) => {
        operator match{
          case "!" => Some(BoolVal(!b))
          case _ => None
        }
      }
      case _ => None
    }

  }

  private def evalReadBool(prompt: String): Option[BoolVal] = {
    print(prompt + " (true | false) => ")
    try {
      Some(BoolVal(scala.io.StdIn.readBoolean()))
    } catch {
      case e: Exception => None
    }
  }

  private def evalReadInt(prompt: String): Option[IntVal] = {
    print(prompt + " => ")
    try {
      Some(IntVal(scala.io.StdIn.readInt()))
    } catch {
      case e: Exception => None
    }
  }

}