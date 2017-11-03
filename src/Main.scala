import graphviz._
import graphviz.io._

sealed trait Stmt
case class Skip() extends Stmt
case class Assign(id: Id, from: IExp) extends Stmt
case class Sequence(left: Stmt, right: Stmt) extends Stmt
case class Conditional(bool: BExp, path_true: Stmt, path_false: Stmt) extends Stmt
case class WhileLoop(bool: BExp, body: Stmt) extends Stmt
case class Assert(check: BExp) extends Stmt

sealed trait BExp
case class LessEqual(a: IExp, b: IExp) extends BExp
case class Equal(a: IExp, b: IExp) extends BExp
case class Not(a: IExp) extends BExp
case class And(a: IExp, b: IExp) extends BExp
case class Or(a: IExp, b: IExp) extends BExp

sealed trait IExp
case class IdealInt(value: BigInt) extends IExp
case class Id(name: String) extends IExp
case class Add(a: IExp, b: IExp) extends IExp
case class Sub(a: IExp, b: IExp) extends IExp

object Main {
    
    def main(args: Array[String]): Unit = {
      generateControlFlow(Skip(), nextNodeId(), nextNodeId());
    }
    
    //return array of tuples representing edges, and the label for each edge
    def generateControlFlow(start_stmt: Stmt, startnode: Int, endnode: Int) : Array[Tuple3[Int,Int,String]] = {
      start_stmt match{
        case _:Skip =>
          return Array();
        case as:Assign =>
          return Array((startnode, endnode, as.id+":="+as.from));
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
        case add: Add => return"("+resolveIExp(add.a)+"+"+resolveIExp(add.b)+")";
        case sub: Sub => return"("+resolveIExp(sub.a)+"-"+resolveIExp(sub.b)+")";
      }
    }
    
    var nodeid_i=0;
    def nextNodeId() : Int = {
      nodeid_i += 1;
      return nodeid_i;
    }
}