import java.io.File
import java.io.PrintWriter
import sys.process._

object solveZ3HornClauses {
  
  def z3Parse(clauses: Array[String]) = {
    val writer = new PrintWriter(new File("Z3HornClauses.txt"));

    writer.write("(set-logic HORN)\n");
    for(line <- clauses){
      writer.write(line+"\n");
    }
    writer.write("(check-sat)");
    writer.close();
    
    val result = "z3 -smt2 Z3HornClauses.txt"!!;
    if(result.stripLineEnd.equals("sat")){
      println("correct");
    }
    else if(result.stripLineEnd.equals("unsat")){
      println("incorrect");
    }
    else{
      println(result);
    }
  }
  
}