

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

object Main {
    
    def main(args: Array[String]): Unit = {
      val prog = Sequence(Sequence(
          Assign(Id("XINT"), IdealInt(15)),
          Assign(Id("YINT"), IdealInt(0))),
          WhileLoop(LessEqual(IdealInt(1), Id("XINT")), 
              Sequence(
                Conditional(Equal(Id("XINT"), IdealInt(5)), Assign(Id("YINT"), IdealInt(1)), Skip()),
                Assign(Id("XINT"), Sub(Id("XINT"), IdealInt(1)))
              )
          ));
      //val parsed_prog = parseImpText.parseTextInput("input.imp");
      //println(parsed_prog);
      val controlflow = generateImpControlFlow.generateControlFlow(prog, generateImpControlFlow.nextNodeId(), generateImpControlFlow.nextNodeId());
      //generateImpControlFlow.generateVisualGraph(controlflow);
      generateImpHornClauses.generateHornClauses(prog, controlflow, true);
    }
}