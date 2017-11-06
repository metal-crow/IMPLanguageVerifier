import graphviz._
import graphviz.graphio._

object generateImpControlFlow {
      def generateVisualGraph(controlflow: Array[Tuple3[Int,Int,String]]) = {
      var edges_graph = Seq[EdgeStatement]();
      for(edge <- controlflow){
        edges_graph = edges_graph :+ EdgeStatement(edge._1.toString, edge._2.toString, Seq[Attribute]("label" := "\""+edge._3+"\"")); 
      }
      val graph = createGraph("tmp", edges_graph);
      saveGraph("tmp2.dot", "tmp2.png", graph);
    }
    
    //return array of tuples representing edges, and the label for each edge
    def generateControlFlow(start_stmt: Stmt, startnode: Int, endnode: Int) : Array[Tuple3[Int,Int,String]] = {
      start_stmt match{
        case _:Skip =>
          return Array((startnode, endnode, ""));
        case as:Assign =>
          return Array((startnode, endnode, as.id.name+":="+resolveIExp(as.from)));
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
          return Array((startnode, left, resolveBExp(con.bool)), (startnode, right, "!"+resolveBExp(con.bool))) ++ ary_true ++ ary_false;
        case whi:WhileLoop =>
          val start_body = nextNodeId();
          val body_edges = generateControlFlow(whi.body, start_body, startnode);
          return Array((startnode, start_body, resolveBExp(whi.bool)), (startnode, endnode, "!"+resolveBExp(whi.bool))) ++ body_edges;
        case asrt:Assert =>
          val bad_boy = nextNodeId();
          return Array((startnode, endnode, resolveBExp(asrt.check)), (startnode, bad_boy, "!"+resolveBExp(asrt.check)));
      }
    }
    
    def resolveBExp(bool: BExp) : String = {
      bool match{
        case le: LessEqual =>
          return resolveIExp(le.a)+"<="+resolveIExp(le.b);
        case eq: Equal =>
          return resolveIExp(eq.a)+"=="+resolveIExp(eq.b);
        case not: Not =>
          return "!"+resolveIExp(not.a);
        case and: And =>
          return resolveIExp(and.a)+" && "+resolveIExp(and.b);
        case or: Or =>
          return resolveIExp(or.a)+" || "+resolveIExp(or.b);
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