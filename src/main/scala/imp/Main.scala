package imp

import ImpParser._

object Main {

  def main(args: Array[String]): Unit = {

    print("Input file name: ")
    val fn = scala.io.StdIn.readLine()
    val parseResult = ImpParser.parsePgrm(fn)

    println
    println("Parsing ... ")
    println

    println("Parse result = " ++ parseResult.toString)
    println
    parseResult match {
      case Success(ast, rest) => {
        println("Executing ... ")
        println
        val finalState = Statement.exec(ast, new State())
        println
        println("Final state = " ++ finalState.toString())
      }
      case failure => println("Failed parse program : " ++ failure.toString)
    }

  }

}