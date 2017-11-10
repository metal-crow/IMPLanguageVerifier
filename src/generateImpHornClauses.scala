

object generateImpHornClauses {

  def generateHornClauses(originalStmt: Stmt, controlflow: Array[Tuple4[Int,Int,String,Tuple2[Either[Stmt, BExp], Boolean]]], printHorn: Boolean) : Array[String] = {
    //generate z3 script
    val z3Script = scala.collection.mutable.ListBuffer[String]();  
    
    //get list of all variables
    val varList = generateVariableList(originalStmt);
    
    //generate z3 text for nodes
    val nodes = scala.collection.mutable.Map[Int, String]();
    for(node <- controlflow){
      nodes(node._1) = "(declare-fun P"+node._1+" ("+List.fill(varList.size)("Int").mkString(" ")+") Bool)";
      nodes(node._2) = "(declare-fun P"+node._2+" ("+List.fill(varList.size)("Int").mkString(" ")+") Bool)";
    }
    
    z3Script ++= nodes.values;
            
    //generate initial start
    val inital_assert_z3 = "(assert "+
                           "(forall ("+varList.map(v => "("+v+" Int)").mkString(" ")+") "+
                           "(=> true (P"+nodes.keySet.reduceLeft(_ min _)+" "+varList.mkString(" ")+") )))";
    val inital_assert_human = "true -> P"+nodes.keySet.reduceLeft(_ min _);
    if(printHorn){
      println(inital_assert_human);
    }
    z3Script += inital_assert_z3;
    
    //generate rest of asserts
    for(edge <- controlflow){
      val node_str = convertNodeToZ3Str(edge._4, edge._1, edge._2, varList);
      if(printHorn){
        println(node_str._2);
      }
      z3Script += node_str._1;
    }
    
    return z3Script.toArray;
  }
  
  //generate set of unique variables from the statement
  def generateVariableList(stmt: Stmt) : Set[String] = {
    stmt match{
        case s:Skip =>
          return Set();
        case as:Assign =>
          return Set(as.id.name) ++ generateVariableList(as.from);
        case se:Sequence =>
          return generateVariableList(se.left)++generateVariableList(se.right);
        case con:Conditional =>
          return generateVariableList(con.bool) ++ generateVariableList(con.path_false) ++ generateVariableList(con.path_true);
        case whi:WhileLoop =>
          return generateVariableList(whi.bool) ++ generateVariableList(whi.body);
        case asrt:Assert =>
          return generateVariableList(asrt.check);
    }
  }
  
  def generateVariableList(bexp: BExp) : Set[String] = {
    bexp match {
      case le: LessEqual =>
        return generateVariableList(le.a) ++ generateVariableList(le.b);
      case eq: Equal =>
        return generateVariableList(eq.a) ++ generateVariableList(eq.b);
      case not: Not =>
        return generateVariableList(not.a);
      case and: And =>
        return generateVariableList(and.a) ++ generateVariableList(and.b);
      case or: Or =>
        return generateVariableList(or.a) ++ generateVariableList(or.b);
    }
  }
  
  def generateVariableList(iexp: IExp) : Set[String] = {
    iexp match {
      case _:IdealInt => return Set();
      case id:Id => return Set(id.name);
      case add:Add => return generateVariableList(add.a) ++ generateVariableList(add.b);
      case sub:Sub => return generateVariableList(sub.a) ++ generateVariableList(sub.b);
    }
  }
  
  
  //Generate Z3 input from the control flow graph
  def convertNodeToZ3Str(exp: Tuple2[Either[Stmt, BExp], Boolean], from: Int, to: Int, varList: Set[String]) : Tuple2[String,String] = {
    val forall_inputs = varList.map(v => "("+v+" Int)").mkString(" ");
    
    exp match{
      case Tuple2(Left(_:Skip),_) => {
        val z3_str = "(assert "+
                     "(forall ("+forall_inputs+") "+
                      "(=> "+
                        "(P"+from+" "+varList.mkString(" ")+")"+
                        "(P"+to+" "+varList.mkString(" ")+")"+
                      ")))";
        val human_str = "P"+from+" -> P"+to;
        return Tuple2(z3_str, human_str);
      }
      case Tuple2(Left(as:Assign),_) => {
        val assign_prime = as.id.name+"Prime";
        val z3_str = "(assert "+
                     "(forall ("+forall_inputs+" ("+assign_prime+" Int)) "+
                     "(=> "+
                       "(and "+
                         "(P"+from+" "+varList.mkString(" ")+")"+
                         "(= "+assign_prime+" "+convertIExpZ3Str(as.from)+")"+
                       ")"+
                       "(P"+to+" "+varList.filter(p => !p.equals(as.id.name)).mkString(" ")+" "+assign_prime+")"+
                     ")))";
        val human_str = "P"+from+" ^ "+assign_prime+"="+convertIExpZ3Str(as.from)+" -> P"+to;
        return Tuple2(z3_str, human_str);
      }
      case Tuple2(Left(_:Conditional),_) | Tuple2(Left(_:Sequence),_) | Tuple2(Left(_:WhileLoop),_) => {
        println("Error: Conditional/Sequence/Loop is not allowed in control flow graph");
        return Tuple2("","");
      }
      case Tuple2(Left(asrt:Assert),true) => {
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
      case Tuple2(Left(asrt:Assert),false) => {
        val z3_str = "(assert "+
                     "(forall ("+forall_inputs+") "+
                     "(=> "+
                       "(and "+
                         "(P"+from+" "+varList.mkString(" ")+")"+
                         convertBExpZ3Str(asrt.check)+
                       ")"+
                       "(P"+to+" "+varList.mkString(" ")+")"+
                     ")))";
        val human_str = "P"+from+" ^ "+convertBExpZ3Str(Not(asrt.check))+" -> P"+to;
        return Tuple2(z3_str, human_str);
      }
      case Tuple2(Right(bexp),_) => {
        val z3_str = "(assert "+
                     "(forall ("+forall_inputs+") "+
                     "(=> "+
                       "(and "+
                         "(P"+from+" "+varList.mkString(" ")+")"+
                         convertBExpZ3Str(bexp)+
                       ")"+
                       "(P"+to+" "+varList.mkString(" ")+")"+
                     ")))";
        val human_str = "P"+from+" ^ "+convertBExpZ3Str(bexp)+" -> P"+to;
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
        case ii: IdealInt => return ii.value.toString;
        case id: Id => return id.name;
        case add: Add => return "(+ "+convertIExpZ3Str(add.a)+" "+convertIExpZ3Str(add.b)+")";
        case sub: Sub => return "(- "+convertIExpZ3Str(sub.a)+" "+convertIExpZ3Str(sub.b)+")";
      }
    }
  
}