import graphviz._
import graphviz.graphio._

object generateImpControlFlow {
      def generateVisualGraph(controlflow: Array[Tuple4[Int,Int,String,Tuple2[Either[Stmt, BExp], Boolean]]]) = {
      var edges_graph = Seq[EdgeStatement]();
      for(edge <- controlflow){
        edges_graph = edges_graph :+ EdgeStatement(edge._1.toString, edge._2.toString, Seq[Attribute]("label" := "\""+edge._3+"\"")); 
      }
      val graph = createGraph("CFGraph", edges_graph);
      println("Generate control flow graph can be found at \"CFGraph.png\"");
      saveGraph("CFGraph.dot", "CFGraph.png", graph);
    }
    
    //return array of tuples representing edges, and the label for each edge
    def generateControlFlow(start_stmt: Stmt, startnode: Int, endnode: Int) : Array[Tuple4[Int,Int,String,Tuple2[Either[Stmt, BExp], Boolean]]] = {
      start_stmt match{
        case s:Skip =>
          return Array((startnode, endnode, "", (Left(s), false)));
        case as:Assign =>
          return Array((startnode, endnode, as.id.name+":="+resolveIExp(as.from), (Left(as), false)));
        case se:Sequence =>
          val middle = nextNodeId();
          val ary = generateControlFlow(se.left, startnode, middle);
          val ary2 = generateControlFlow(se.right, middle, endnode);
          return ary++ary2;
        case con:Conditional =>
          val left = nextNodeId();
          val right = nextNodeId();
          val ary_true = generateControlFlow(con.path_true, left, endnode);
          val ary_false = generateControlFlow(con.path_false, right, endnode);
          return Array( (startnode, left, resolveBExp(con.bool), (Right(con.bool), false)), (startnode, right, "!"+resolveBExp(con.bool), (Right(Not(con.bool)), false)) ) ++ ary_true ++ ary_false;
        case whi:WhileLoop =>
          val start_body = nextNodeId();
          val body_edges = generateControlFlow(whi.body, start_body, startnode);
          return Array( (startnode, start_body, resolveBExp(whi.bool), (Right(whi.bool), false)), (startnode, endnode, "!"+resolveBExp(whi.bool), (Right(Not(whi.bool)), false)) ) ++ body_edges;
        case asrt:Assert =>
          val bad_boy = nextNodeId();
          return Array( (startnode, endnode, resolveBExp(asrt.check), (Left(asrt), false)), (startnode, bad_boy, "!"+resolveBExp(asrt.check), (Left(asrt), true)) );
      }
    }
    
    def resolveBExp(bool: BExp) : String = {
      bool match{
        case le: LessEqual =>
          return resolveIExp(le.a)+"<="+resolveIExp(le.b);
        case eq: Equal =>
          return resolveIExp(eq.a)+"=="+resolveIExp(eq.b);
        case not: Not =>
          return "!"+resolveBExp(not.a);
        case and: And =>
          return resolveBExp(and.a)+" && "+resolveBExp(and.b);
        case or: Or =>
          return resolveBExp(or.a)+" || "+resolveBExp(or.b);
      }
    }
    
    def resolveIExp(in: IExp) : String = {
      in match{
        case ii: IdealInt => return ii.value.toString;
        case id: Id => return id.name;
        case add: Add => return "("+resolveIExp(add.a)+"+"+resolveIExp(add.b)+")";
        case sub: Sub => return "("+resolveIExp(sub.a)+"-"+resolveIExp(sub.b)+")";
      }
    }
    
    var nodeid_i=0;
    def nextNodeId() : Int = {
      nodeid_i += 1;
      return nodeid_i;
    }
}