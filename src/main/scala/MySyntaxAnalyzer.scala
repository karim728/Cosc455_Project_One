
//Syntax Analyzer List of tokens from Compiler and is used here to navigate the file.
// Each production rule is its own method.

class MySyntaxAnalyzer extends SyntaxAnalyzer {
  var Tree = new scala.collection.mutable.Stack[String]
  var TextBool = false
  var innerItemFound = false;
  var count = 0;
  override def gittex() : Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCB)) {
      Tree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      title()
      variableDefine()
      body()
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCE)) {
        Tree.push(Compiler.currentToken)
        if (Compiler.Readed_file.length - Compiler.position > 15) {
          println("SYNTAX ERROR: Must be most be before \\END Tag")
          System.exit(1)
        }
      }
    } else {
      println("SYNTAX ERROR :  HTML Tag not found ")
      System.exit(1)
    }
  }

  override def title() : Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.TITLEB)){
      Tree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      if(TextBool) {
        Tree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }else {
        println("SYNTAX ERROR: Text Required")
        System.exit(1)
      }
      if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)){
        Tree.push(Compiler.currentToken)
      }else{
        println("Ending Bracket not found")
        System.exit(1)
      }
    }
  }

  override def body(): Unit = {
    innerText()
    paragraph()
    newline()
    while(!Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCE)) {
      Compiler.Scanner.getNextToken()
      body()
    }
  }
  override def paragraph(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARB)) {
      Tree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      variableDefine()
      innerText()
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARE)) {
        Tree.push(Compiler.currentToken)
      } else {
        println("SYNTAX ERROR : SYNTAX Rules not followed!")
        System.exit(1)
      }
    }
  }
  override def innerText(): Unit = {
    Compiler.currentToken match{
      case CONSTANTS.DEFB => variableDefine(); innerText()
      case CONSTANTS.USEB => variableUse();  innerText()
      case CONSTANTS.HEADING => heading(); innerText()
      case CONSTANTS.BOLD => bold(); innerText()
      case CONSTANTS.ITALICS =>italics(); innerText()
      case CONSTANTS.LISTITEM =>listItem(); innerText()
      case CONSTANTS.IMAGEB =>image(); innerText()
      case CONSTANTS.LINKB => link(); innerText()
      case CONSTANTS.NEWLINE =>newline(); innerText()
      case _ =>{
        if(TextBool){
          text();
          innerText()
        }
      }
    }

  }


  override def heading(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.HEADING)) {
      Tree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      if(TextBool) {
        Tree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }else {
        println("SYNTAX ERROR: Text Required")
        System.exit(1)
      }

    }
  }

  override def variableDefine(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DEFB)){
      Tree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      if(TextBool) {
        Tree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }else {
        println("SYNTAX ERROR: Text Required")
        System.exit(1)
      }
      if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.EQSIGN)){
        Tree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }else{
        println("SYNTAX ERROR: Variable Define '=' not found")
        System.exit(1)}
      if(TextBool) {
        Tree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }else {
        println("SYNTAX ERROR: Text Required")
        System.exit(1)
      }
      if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)){
        Tree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
        variableDefine()
      }else{
        println("SYNTAX ERROR: Ending Bracket not found")
        System.exit(1)
      }
    }
  }

  override def variableUse(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB)){
      Tree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      if(TextBool){
        text()
      }else{
        println("SYNTAX ERROR: Variable Use Text Required")
        System.exit(1)
      }
      if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)){
        Tree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
        variableUse()
      }else{
        println("SYNTAX ERROR: Variable Use Ending Tag Required")
        System.exit(1)
      }
    }
  }

  override def bold(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD)){
      Tree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      if(TextBool){
        text()
      }
      if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD)){
        Tree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }else{
        println("SYNTAX ERROR: Bold Ending Tag Required")
        System.exit(1)
      }
    }
  }

   def italics(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ITALICS)){
      Tree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      if(TextBool){
        text()
      }
      if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ITALICS)){
        Tree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }else{
        println("SYNTAX ERROR: Italics Ending Tag Required")
        System.exit(1)
      }
    }
  }

  override def listItem(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LISTITEM)){
      Tree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      innerItem()
      listItem()
    }
  }

   def innerItem(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB)){
      innerItemFound = true
      variableUse()
      innerItem()
    }else if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD)){
      innerItemFound = true
      bold()
      innerItem()
    }else if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ITALICS)){
      innerItemFound = true
      italics()
      innerItem()
    }else if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LINKB)){
      innerItemFound = true
      link()
      innerItem()
    }else{
      if(!innerItemFound) {
        if (TextBool) {
          text()
        } else {
          println("SYNTAX ERROR: Text Required"); System.exit(1)
        }
      }
    }
  }

  override def link(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LINKB)){
      Tree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      if(TextBool) {
        Tree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }else {
        println("SYNTAX ERROR: Text Required")
        System.exit(1)
      }
      if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)){
        Tree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }else{
        println("SYNTAX ERROR: Ending Bracket Required for Link")
        System.exit(1)
      }
      if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSB)){
        Tree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }else{
        println("SYNTAX ERROR: Address required after the description ")
        System.exit(1)
      }
      if(TextBool) {
        Tree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }else {
        println("SYNTAX ERROR: Text Required")
        System.exit(1)
      }
      if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSE)){
        Tree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
        if(TextBool) {
          Tree.push(Compiler.currentToken)
          Compiler.Scanner.getNextToken()
        }else {
          println("SYNTAX ERROR: Text Required")
          System.exit(1)
        }
        if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSE)){
          Tree.push(Compiler.currentToken)
          Compiler.Scanner.getNextToken()
        }
      }else{
        println("SYNTAX ERROR: Ending Link Symbol required")
        System.exit(1)
      }

    }
  }

  override def image(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.IMAGEB)){
      Tree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      if(TextBool){
        text()
      }else{println("SYNTAX ERROR: Image Text Required");System.exit(1)}
      if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)){
        Tree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }else{println("SYNTAX ERROR: Image Ending Bracket Before Link Required");System.exit(1)}
      if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSB)){
        Tree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
        if(TextBool){
          text()
        }else{println("SYNTAX ERROR: Image Address Required");System.exit(1)}
        if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSE)){
          Tree.push(Compiler.currentToken)
          Compiler.Scanner.getNextToken()
        }else{
          println("SYNTAX ERROR: Image Ending Address Token Required")
        }
      }else{println("SYNTAX ERROR: Image Address Begin Bracket Required")}
    }
  }

  override def newline(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.NEWLINE)){
      Tree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
    }
  }

  def text():Unit ={
    while(TextBool){
      Tree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
    }
  }
  def retTree: scala.collection.mutable.Stack[String]={
    return Tree;
  }


}