import scala.io.Source

object parseImpText {
  
    def parseTextInput(filename: String) : Option[Stmt] = {
      val input_lines = Source.fromFile(filename)
                          .mkString
                          .split("\n")
                          .map(l => l.replaceAll("\\s",""));
      
      return parseTextInputLines(input_lines)._1;
    }
  
    def valid_id_regex = "^[a-zA-Z][a-zA-Z0-9_]*$";
    //return statement + line index ended at
    def parseTextInputLines(input_lines: Array[String]) : Tuple2[Option[Stmt], Integer] = {
      var final_stmt: Stmt = Skip();
      
      var i:Integer=0;
      while(i < input_lines.length){
        var line = input_lines(i);
        //println(line);
        
        //Skip
        if(line.toLowerCase().equals("skip")){
          final_stmt = Sequence(final_stmt, Skip());
        }
        
        //Assign
        if(line.contains(":=")){
          //check valid id
          val id = line.substring(0, line.indexOf(":="));
          if(!id.matches(valid_id_regex)){
            println("Variable \""+id+"\" is an invalid variable name");
            return (None, i);
          }
          //check valid iexp
          val iexp_str = line.substring(line.indexOf(":=")+2, line.length());
          val iexp = parseIExp(iexp_str);
          if(iexp.isEmpty){
            println("Assignment \""+iexp_str+"\" is an invalid assignment");
            return (None, i);
          }
          final_stmt = Sequence(final_stmt, Assign(Id(id), iexp.get));
        }
        
        //Conditional
        if(line.toLowerCase().startsWith("if(") && line.contains(")")){
          val if_cond_str = line.substring(3, line.indexOf(")"));
          val if_cond = parseBExp(if_cond_str);
          if(if_cond.isEmpty){
            println("If statement \""+if_cond_str+"\" is invalid");
            return (None, i);
          }
          i += 1;
          val true_restuple = parseTextInputLines(input_lines.slice(i, input_lines.length));
          i += true_restuple._2;
          val path_true =  true_restuple._1
          if(path_true.isEmpty){
            return (None, i);
          }
          i += 1;
          val false_restuple = parseTextInputLines(input_lines.slice(i, input_lines.length));
          i += false_restuple._2;
          val path_false =  false_restuple._1
          if(path_false.isEmpty){
            return (None, i);
          }
                 
          final_stmt = Sequence(final_stmt, Conditional(if_cond.get, path_true.get, path_false.get));
        }
        
        //recursion termination cases for conditional
        if(line.toLowerCase().equals("else") || line.toLowerCase().equals("endif")){
          return (Some(final_stmt), i);
        }
        
        //while loop
        if(line.toLowerCase().startsWith("while(") && line.contains(")")){
          val while_cond_str = line.substring(6, line.indexOf(")"));
          val while_cond = parseBExp(while_cond_str);
          if(while_cond.isEmpty){
            println("while statement \""+while_cond_str+"\" is invalid");
            return (None, i);
          }
          i += 1;

          val body_restuple = parseTextInputLines(input_lines.slice(i, input_lines.length));
          i = body_restuple._2;
          val body =  body_restuple._1
          if(body.isEmpty){
            return (None, i);
          }

          final_stmt = Sequence(final_stmt, WhileLoop(while_cond.get, body.get));
        }
        
        //recursion termination cases for while
        if(line.toLowerCase().equals("endwhile")){
          return (Some(final_stmt), i);
        }
        
        //assert
        if(line.toLowerCase().startsWith("assert(") && line.contains(")")){
          val assert_cond_str = line.substring(7, line.indexOf(")"));
          val assert_cond = parseBExp(assert_cond_str);
          if(assert_cond.isEmpty){
            println("assert statement \""+assert_cond_str+"\" is invalid");
            return (None, i);
          }

          final_stmt = Sequence(final_stmt, Assert(assert_cond.get));          
        }
        
        i += 1;
      }
      
      return (Some(final_stmt), i);
    }
    
    def parseBExp(bexp: String) : Option[BExp] = { 
      //println("BEXP "+bexp);
      
      //check not
      if(bexp.startsWith("!")){
        val inner_str = bexp.substring(1, bexp.length());
        val inner = parseBExp(inner_str);
        if(inner.isEmpty){
          println("Invalid not \""+inner_str+"\"");
          return None;
        }
        return Some(Not(inner.get));
      }
      
      //split on <=, ==, AND, OR but preserve delimeter
      val splitbexp = bexp.split("(?<=<=)|(?=<=)|(?<===)|(?===)|(?<=&&)|(?=&&)|(?<=\\|\\|)|(?=\\|\\|)");
      val i = splitbexp.length/2;
      
      val left_str = splitbexp.slice(0, i).mkString("");
      val right_str = splitbexp.slice(i+1, splitbexp.length).mkString("");
      
      splitbexp(i) match{
        case "<=" => {
          val left = parseIExp(left_str);
          val right = parseIExp(right_str);
          if(left.isDefined && right.isDefined){
            return Some(LessEqual(left.get, right.get));
          }
        }
        case "==" => {
          val left = parseIExp(left_str);
          val right = parseIExp(right_str);
          if(left.isDefined && right.isDefined){
            return Some(Equal(left.get, right.get));
          }
        }
        case "&&" => {
          val left = parseBExp(left_str);
          val right = parseBExp(right_str);
          if(left.isDefined && right.isDefined){
            return Some(And(left.get, right.get));
          }
        }
        case "||" => {
          val left = parseBExp(left_str);
          val right = parseBExp(right_str);
          if(left.isDefined && right.isDefined){
            return Some(Or(left.get, right.get));
          }
        }
        case _ => {
          println("ERROR :"+splitbexp(i));
          return None;
        }
      }      
            
      println("Unable to parse BExp \""+bexp+"\"");
      return None;
    }
    
    def parseIExp(iexp: String) : Option[IExp] = {
      //println("IEXP "+iexp);
      
      //check if int
      if(iexp.matches("^\\d+$")){
        return Some(IdealInt(BigInt(iexp)));
      }
      //check if id
      if(iexp.matches(valid_id_regex)){
        return Some(Id(iexp));
      }
      
      //split on +, - but preserve delimeter
      val splitiexp = iexp.split("(?<=\\+)|(?=\\+)|(?<=-)|(?=-)");
      val i = splitiexp.length/2;
      
      val left_str = splitiexp.slice(0, i).mkString("");
      val right_str = splitiexp.slice(i+1, splitiexp.length).mkString("");

      splitiexp(i) match{
        case "+" => {
          val left = parseIExp(left_str);
          val right = parseIExp(splitiexp.slice(i+1, splitiexp.length).mkString(""));
          if(left.isDefined && right.isDefined){
            return Some(Add(left.get, right.get));
          }
        }
        case "-" => {
          val left = parseIExp(left_str);
          val right = parseIExp(right_str);
          if(left.isDefined && right.isDefined){
            return Some(Sub(left.get, right.get));
          }
        }
        case _ => {
          println("ERROR :"+splitiexp(i));
          return None;
        }
      }
      
      println("Unable to parse IExp \""+iexp);
      return None;
    }
}