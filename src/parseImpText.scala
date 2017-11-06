import scala.io.Source

object parseImpText {
    def valid_id_regex = "^[a-zA-Z][a-zA-Z0-9_]*$";
    def parseTextInput(filename: String) : Option[Stmt] = {
      var final_stmt: Stmt = Skip();
      
      val input_lines = Source.fromFile(filename).mkString.split("\n");
      input_lines.foreach(l => l.replaceAll("\\s+"," "));
      
      var i=0;
      while(i < input_lines.length){
        val line = input_lines(i);
        //ugly parsing
        
        //Skip
        if(line.toLowerCase().contains("skip;")){
          final_stmt = Sequence(final_stmt, Skip());
        }
        //Assign
        if(line.contains(":=")){
          //check valid id
          val id = line.substring(0, line.indexOf(":="));
          if(id.matches(valid_id_regex)){
            println("Variable \""+id+"\" is an invalid variable name");
            return None;
          }
          //check valid iexp
          val iexp_str = line.substring(line.indexOf(":=")+2, line.length());
          val iexp = parseIExp(iexp_str);
          if(iexp.isEmpty){
            println("Assignment \""+iexp_str+"\" is an invalid assignment");
            return None;
          }
          final_stmt = Sequence(final_stmt, Assign(Id(id), iexp.get));
        }
        //Conditional
        if(line.startsWith("if(")){
          
        }
      }
      
      return Some(final_stmt);
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
        val splitParse = helperIExpSplitParse(bexp, and_i, 2);
        if(splitParse.isEmpty){
          return None;
        }
        return Some(And(splitParse.get._1, splitParse.get._2));
      }
      
      val or_i = bexp.indexOf("||");      
      if(or_i>=0){
        val splitParse = helperIExpSplitParse(bexp, or_i, 2);
        if(splitParse.isEmpty){
          return None;
        }
        return Some(Or(splitParse.get._1, splitParse.get._2));
      }
      
      //check not
      
            
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
}