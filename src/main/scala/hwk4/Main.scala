
package hwk4;

import java.io.File

import hwk.{GenerateAST, Statement}
 

object Main {
  def main(args: Array[String]) { 
    val ast = GenerateAST(new File("test/rd1.js"))
	 // val ast = GenerateAST(new File("test/lv.js"))
	  
    ast.prep 
    ast.buildGraph
    
    //val a = RD(ast)
    val a = LV(ast)
    
    a.worklist
    
    println(a)
    
    println
    
    print(a.toDotGraph)
  }
}


