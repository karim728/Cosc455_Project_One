
//Compiler will accepts runtime argument of the file name to read, and read its contents using the a method. After this is validated,
//it should continue and loop through the contents of the file, tokenizing them with Scanner.getNextToken.
object  Compiler {

  val Scanner = new MyLexicalAnalyzer
  val Parser = new MySyntaxAnalyzer

  val SemanticAnalyzer  = new MySemanticAnalyzer
  var position: Int = -1
  var filename: String = ""
  var Readed_file : String = ""
  var currentToken : String = ""
  def main(args: Array[String]) = {
    filename = args(0)
    checkFile(args)
    readFile(args(0))

    Scanner.getNextToken()
    Parser.gittex()

    SemanticAnalyzer .convertCode()

  }
  def readFile(file : String) = {

    var source = scala.io.Source.fromFile(file)
    Readed_file = try source.mkString finally source.close()
  }

  def checkFile(args: Array[String]) = {
    if (args.length != 1 ){
      println("Usage Error: wrong number of args fool!")
      System.exit(1)
    }
    else if(!args(0).endsWith((".gtx"))){
      println("Usage Error: wrong extension fool!")
      System.exit(1)
    }
  }
}