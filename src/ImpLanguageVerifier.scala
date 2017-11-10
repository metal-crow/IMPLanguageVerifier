import java.nio.file.{Paths, Files}

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
case class Not(a: BExp) extends BExp
case class And(a: BExp, b: BExp) extends BExp
case class Or(a: BExp, b: BExp) extends BExp

sealed trait IExp
case class IdealInt(value: BigInt) extends IExp
case class Id(name: String) extends IExp
case class Add(a: IExp, b: IExp) extends IExp
case class Sub(a: IExp, b: IExp) extends IExp

object ImpLanguageVerifier {
    
    def main(args: Array[String]): Unit = {
       if (args.length == 0) {
        println("Incorrect usage. Call using \"NAME [-c][-h] <sourcefile>.imp\"");
        return;
      }
      var filename = "";
      var generateControlFlowGraphDiagram = false;
      var printHornClauses = false;
      
      for(arg <- args){
        if(arg.equals("-c")){
          generateControlFlowGraphDiagram = true;
        }
        if(arg.equals("-h")){
          printHornClauses=true;
        }
        if(arg.endsWith(".imp")){
          //check filename
          if(Files.exists(Paths.get(arg))){
            filename = arg;
          }
        }
      }
      
      if(filename.equals("")){
        println("No filename supplied");
        return;
      }
      
      //read in Imp
      val parsed_prog = parseImpText.parseTextInput("input.imp");
      if(parsed_prog.isEmpty){
        return;
      }
      val prog = parsed_prog.get;
      //println(prog);
      
      //Generate control flow graph
      val controlflow = generateImpControlFlow.generateControlFlow(prog, generateImpControlFlow.nextNodeId(), generateImpControlFlow.nextNodeId());
      if(generateControlFlowGraphDiagram){
        generateImpControlFlow.generateVisualGraph(controlflow);
      }
      
      //Generate and parse Horn Clauses
      val z3HornClauses = generateImpHornClauses.generateHornClauses(prog, controlflow, printHornClauses);
      solveZ3HornClauses.z3Parse(z3HornClauses);
    }
}