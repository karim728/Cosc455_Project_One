//In this class, The Lexical Analyzer Check first char of token in getNextToken to see if it needs text,
//special or whitespace processing. Skip whitespace. For text, put consecutive chars of text into
// a charArray, and that is one token. For special, go on a case by case basis "matching"
//the first character of the token.


class MyLexicalAnalyzer extends LexicalAnalyzer{

  var current : Char = ' '
  var Text : Boolean = false
  var BoolCheck : Boolean = false
  val lexems: List[String] = List("\\BEGIN","\\END", "\\TITLE[","]", "#", "\\PARAB",
    "\\PARAE", "*", "+", "\\\\", "\\", "[", "(", ")", "![",
    "\\DEF[", "=", "\\USE[", "**")
  var tokenString = ""
  override def addChar(): Unit = {
    tokenString = tokenString + current
  }

  override def getChar(): Char = {
    Compiler.position+=1
    Compiler.Readed_file.charAt(Compiler.position)
  }
  override def getNextToken(): Unit = {
    Compiler.Parser.TextBool = false
    if (isSpace()) {
      current = getChar()
      while (isSpace()) {
        current = getChar()
      }
    }
    if(current.equals('=') || (current.equals(']') || current.equals('[') || current.equals('+') || current.equals('(') || current.equals(')') )){
      Compiler.currentToken = current.toString
      if(current.equals('=') || (current.equals(']') || current.equals('[') || current.equals('+')|| current.equals('(') || current.equals(')') )) current = getChar()
    }else if(isSpecial()){
      addChar()
      current = getChar()
      while(!endChar()){
        addChar()
        current = getChar()
      }
      if(lexems.contains(current.toString)) {
        addChar()
      }
      if (lookup()) {
        Compiler.currentToken = tokenString.toUpperCase()
        tokenString = ""
        current = getChar()
      } else {
        println("Lexical Error: " + tokenString + " is not valid!")
        System.exit(1)
      }
    }else if(text()){
      while(text()){Compiler.Parser.TextBool = true; addChar(); current = getChar();}
      Compiler.currentToken = tokenString
      tokenString = ""
    }else{
      println("Lexical Error: Cannot find Lexems")
      System.exit(1)
    }
  }
  def isSpace(): Boolean = {
    if(current.equals('\r') || current.equals('\n') || current.equals(' ') || current.equals('\t')){
      true
    }else
      false
  }
  def isSpecial(): Boolean = {
    current match{
      case '\\' | '*' | '#' | '+' | '[' | '!' | '(' => true
      case _ => false
    }
  }
  def endChar(): Boolean = {
    current match {
      case '\r' | '\n' | '[' | '\\' | ']' | '*' | ')' | '(' | ' ' | ':' | '\t' => true
      case _ => false
    }
  }
  def text() : Boolean = {
    Text = false
    for(character <- 'A' to 'Z'){
      if(current.equals(character)) {
        Text = true
      }
    }
    for(number <- '0' to '9'){
      if(current.equals(number))
        Text = true
    }
    for(character <- 'a' to 'z'){
      if(current.equals(character))
        Text = true
    }
    if(current.equals(',') || current.equals('.')|| current.equals('\"')|| current.equals('.')
      || current.equals('?')|| current.equals('_')|| current.equals('/') || current.equals(' ') || current.equals(':'))
      Text = true

    Text
  }
  def lookup(): Boolean = {
    var flag = false
    if(lexems.contains(tokenString.toUpperCase()))
      flag = true;
    flag
  }

}