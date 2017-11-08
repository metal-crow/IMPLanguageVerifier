

object generateImpHornClauses {
  
  def generateHornClauses(controlflow: Array[Tuple4[Int,Int,String,Either[Stmt, BExp]]]) : Array[String] = {
    //generate z3 text for nodes
    val nodes = scala.collection.mutable.Map[Int, String]();
    for(node <- controlflow){
      nodes(node._1) = "(declare-fun P"+node._1+" (Int) Bool)";
      nodes(node._2) = "(declare-fun P"+node._2+" (Int) Bool)";
    }
    
    //generate z3 asserts
    val asserts = scala.collection.mutable.ListBuffer[String]();    
    //initalize the first clause
    asserts += "(assert (forall ((n Int)) (=> (= n 0) (P1 n) )))"
  }
  
  def convertStmtToZ3Str(exp: Stmt, from: Int, to: Int, varList: Array[String]) : String = {
    exp match{
      case _:Skip => return "(assert  (=> (true) (P"+to+" "+varList.mkString(",")+") ))";
      case as:Assign => {
        val assign_prime = as.id+"Prime";
        return "(assert "+
               "(forall (("+as.id+" Int) ("+assign_prime+" Int)) "+
               "(=> "+
                 "(and "+
                   "(P"+from+" "+varList.mkString(",")+")"+
                   //exp
                 ")"+
                 "(P"+to+" "+varList.filter(p => !p.equals(as.id)).mkString(",")+","+assign_prime+")"+
               ")))";
      }
      
    }
  }
}