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
        println(line);
        
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
      //check if <=, ==, AND, OR
      val lessequal_i = bexp.indexOf("<=");
      if(lessequal_i>=0){
        val splitParse = helperIExpSplitParse(bexp, lessequal_i, 2);
        if(splitParse.isEmpty){
          return None;
        }
        return Some(LessEqual(splitParse.get._1, splitParse.get._2));
      }
      
      val equal_i = bexp.indexOf("==");
      if(equal_i>=0){
        val splitParse = helperIExpSplitParse(bexp, equal_i, 2);
        if(splitParse.isEmpty){
          return None;
        }
        return Some(Equal(splitParse.get._1, splitParse.get._2));
      }
            
      val and_i = bexp.indexOf("&&");
      if(and_i>=0){
        val splitParse = helperBExpSplitParse(bexp, and_i, 2);
        if(splitParse.isEmpty){
          return None;
        }
        return Some(And(splitParse.get._1, splitParse.get._2));
      }
      
      val or_i = bexp.indexOf("||");      
      if(or_i>=0){
        val splitParse = helperBExpSplitParse(bexp, or_i, 2);
        if(splitParse.isEmpty){
          return None;
        }
        return Some(Or(splitParse.get._1, splitParse.get._2));
      }
      
      //check not
      val not_i = bexp.indexOf("!");
      if(not_i>=0){
        val inner_str = bexp.substring(not_i);
        val inner = parseBExp(inner_str);
        if(inner.isEmpty){
          println("Invalid not \""+inner_str+"\"");
          return None;
        }
        return Some(Not(inner.get));
      }      
            
      println("Unable to parse BExp \""+bexp+"\"");
      return None;
    }
    
    def parseIExp(iexp: String) : Option[IExp] = {
      //check if int
      if(iexp.matches("^\\d+$")){
        return Some(IdealInt(BigInt(iexp)));
      }
      //check if id
      if(iexp.matches(valid_id_regex)){
        return Some(Id(iexp));
      }
      //check if addition or subtraction (this innatly preserves left to right eval)
      val plus_i = iexp.indexOf("+");
      if(plus_i>=0){
        val splitParse = helperIExpSplitParse(iexp, plus_i, 1);
        if(splitParse.isEmpty){
          return None;
        }
        return Some(Add(splitParse.get._1, splitParse.get._2));
      }
      
      val minus_i = iexp.indexOf("-");
      if(minus_i>=0){
        val splitParse = helperIExpSplitParse(iexp, minus_i, 1);
        if(splitParse.isEmpty){
          return None;
        }
        return Some(Sub(splitParse.get._1, splitParse.get._2));
      }
      
      println("Unable to parse IExp \""+iexp);
      return None;
    }
    
    def helperIExpSplitParse(str: String, split: Int, splitlen: Int) : Option[Tuple2[IExp, IExp]] = {
        val left = str.substring(0, split);
        val left_iexp = parseIExp(left);
        if(left_iexp.isEmpty){
            println("IExp \""+left+"\" is an invalid expression");
            return None;
        }
        val right = str.substring(split+splitlen, str.length());
        val right_iexp = parseIExp(right);
        if(right_iexp.isEmpty){
            println("IExp \""+right+"\" is an invalid expression");
            return None;
        }
        
        return Some(left_iexp.get, right_iexp.get);
    }
    
    def helperBExpSplitParse(str: String, split: Int, splitlen: Int) : Option[Tuple2[BExp, BExp]] = {
        val left = str.substring(0, split);
        val left_bexp = parseBExp(left);
        if(left_bexp.isEmpty){
            println("BExp \""+left+"\" is an invalid expression");
            return None;
        }
        val right = str.substring(split+splitlen, str.length());
        val right_bexp = parseBExp(right);
        if(right_bexp.isEmpty){
            println("BExp \""+right+"\" is an invalid expression");
            return None;
        }
        
        return Some(left_bexp.get, right_bexp.get);
    }
}