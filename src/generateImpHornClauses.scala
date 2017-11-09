

object generateImpHornClauses {
  
  def main(args: Array[String]): Unit = {
    
  }
  
  def generateHornClauses(controlflow: Array[Tuple4[Int,Int,String,Either[Stmt, BExp]]]) : Array[String] = {
    //generate z3 text for nodes
    val nodes = scala.collection.mutable.Map[Int, String]();
    for(node <- controlflow){
      nodes(node._1) = "(declare-fun P"+node._1+" (Int) Bool)";
      nodes(node._2) = "(declare-fun P"+node._2+" (Int) Bool)";
    }
    
    //generate z3 asserts
    val asserts = scala.collection.mutable.ListBuffer[String]();  
    //generate inital start
    
    //generate rest of asserts
  }
  
  def convertNodeToZ3Str(exp: Either[Stmt, BExp], from: Int, to: Int, varList: Array[String]) : Tuple2[String,String] = {
    val forall_inputs = varList.map(v => "( "+v+" Int)").mkString(" ");
    
    exp match{
      case Left(_:Skip) => {
        val z3_str = "(assert  "+
                     "(forall ("+forall_inputs+") "+
                      "(=> "+
                        "(P"+from+" "+varList.mkString(" ")+")"+
                        "(P"+to+" "+varList.mkString(" ")+")"+
                      ")))";
        val human_str = "P"+from+" -> P"+to;
        return Tuple2(z3_str, human_str);
      }
      case Left(as:Assign) => {
        val assign_prime = as.id+"Prime";
        val z3_str = "(assert "+
                     "(forall ("+forall_inputs+"("+assign_prime+" Int)) "+
                     "(=> "+
                       "(and "+
                         "(P"+from+" "+varList.mkString(" ")+")"+
                         "(= "+assign_prime+" "+convertIExpZ3Str(as.from)+")"
                       ")"+
                       "(P"+to+" "+varList.filter(p => !p.equals(as.id)).mkString(" ")+" "+assign_prime+")"+
                     ")))";
        val human_str = "P"+from+" ^ "+assign_prime+"="+convertIExpZ3Str(as.from)+" -> P"+to;
        return Tuple2(z3_str, human_str);
      }
      case Left(_:Conditional) | Left(_:Sequence) | Left(_:WhileLoop) => {
        println("Error: Conditional/Sequence/Loop is not allowed in control flow graph");
        return Tuple2("","");
      }
      case Left(asrt:Assert) => {
        val z3_str = "(assert "+
                     "(forall ("+forall_inputs+") "+
                     "(=> "+
                       "(and "+
                         "(P"+from+" "+varList.mkString(" ")+")"+
                         convertBExpZ3Str(Not(asrt.check))+
                       ")"+
                       "false"+
                     ")))";
        val human_str = "P"+from+" ^ "+convertBExpZ3Str(Not(asrt.check))+" -> P"+to;
        return Tuple2(z3_str, human_str);
      }
      case Right(bexp) => {
        val z3_str = "(assert "+
                     "(forall ("+forall_inputs+") "+
                     "(=> "+
                       "(and "+
                         "(P"+from+" "+varList.mkString(" ")+")"+
                         convertBExpZ3Str(bexp)+
                       ")"+
                       "(P"+to+" "+varList.mkString(" ")+")"+
                     ")))";
        val human_str = "P"+from+" ^ "+convertBExpZ3Str(bexp)+" -> P"+to;//TODO
        return Tuple2(z3_str, human_str);
      }
    }
  }
  
  def convertBExpZ3Str(in: BExp) : String = {
    in match{
      case le:LessEqual => return "(<= "+convertIExpZ3Str(le.a)+" "+convertIExpZ3Str(le.b)+")";
      case eq:Equal => return "(= "+convertIExpZ3Str(eq.a)+" "+convertIExpZ3Str(eq.b)+")";
      case no:Not => return "(not "+convertBExpZ3Str(no.a)+")";
      case and:And => return "(and "+convertBExpZ3Str(and.a)+" "+convertBExpZ3Str(and.b)+")";
      case or:Or => return "(or "+convertBExpZ3Str(or.a)+" "+convertBExpZ3Str(or.b)+")";
    }
  }
  
   def convertIExpZ3Str(in: IExp) : String = {
      in match{
        case ii: IdealInt => return "("+ii.value.toString+")";
        case id: Id => return "("+id.name+")";
        case add: Add => return "(+"+convertIExpZ3Str(add.a)+" "+convertIExpZ3Str(add.b)+")";
        case sub: Sub => return "(+"+convertIExpZ3Str(sub.a)+" "+convertIExpZ3Str(sub.b)+")";
      }
    }
  
}