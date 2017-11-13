import java.awt.Desktop
import java.io.{File, IOException, PrintWriter}
import scala.collection.mutable


//MySemanticAnalyzer  pops tokens off of the parseTreeStack as it constructs a new stack,
//and also it will check that a variable has been declared before it can be used.

class MySemanticAnalyzer {
  val Tree = Compiler.Parser.Tree
  var varStack = new mutable.Stack[String]
  var printTree : List[String] = Nil
  var varOn = ""
  var liBool = false
  var booleanLi = false
  var scope = ""
  def convertCode(): Unit = {
    var resTree = Tree.clone().reverse
    var resVarStack = varStack.clone()
    var current = resTree.pop()
    while(!resTree.isEmpty) {
      current match {
        case CONSTANTS.DOCB => printTree = "<!DOCTYPE html>\n<html>\n<head>\n" :: printTree; scope = "Begin"; current = resTree.pop();
        case CONSTANTS.DOCE => if(varOn.equalsIgnoreCase("list")){printTree = "</li>"::printTree}; printTree = "</body>\n</html>" :: printTree; current = resTree.pop();
        case CONSTANTS.TITLEB => {
          current = resTree.pop()
          var Text = current
          current = resTree.pop()
          printTree = "<body>\n" :: "</head>\n" :: "</title>\n" :: Text :: "<title>" :: printTree
          current = resTree.pop()
        }
        case CONSTANTS.HEADING => {
          printTree = "<h1>" :: printTree
          current = resTree.pop()
          printTree = current :: printTree
          printTree = "</h1>\n" :: printTree
          current = resTree.pop()
        }
        case CONSTANTS.PARB => printTree = "\n<p>" :: printTree; scope = "PARAB"; varOn = "PARAB"; current = resTree.pop();
        case CONSTANTS.PARE => printTree = "</p>\n" :: printTree; scope = "Begin"; current = resTree.pop();
        case CONSTANTS.BOLD => {
          printTree = "<b>" :: printTree;
          current = resTree.pop();
          while (!current.equalsIgnoreCase(CONSTANTS.BOLD)) {
            printTree = current :: printTree
            current = resTree.pop()
          }
          printTree = "</b>\n" :: printTree
          current = resTree.pop()
        }
        case CONSTANTS.ITALICS => {
          printTree = "<i>" :: printTree
          current = resTree.pop()
          while (!current.equalsIgnoreCase(CONSTANTS.ITALICS)) {
            printTree = current :: printTree
            current = resTree.pop()
          }
          printTree = "</i>\n" :: printTree
          current = resTree.pop()
        }
        case CONSTANTS.LISTITEM => {
          if(liBool){printTree = "</li>\n"::printTree}
          printTree ="<li>" :: printTree
          varOn = "list"
          liBool = true
          current = resTree.pop()


        }
        case CONSTANTS.NEWLINE => printTree = "<br>\n" :: printTree; current = resTree.pop();
        case CONSTANTS.LINKB => {
          current = resTree.pop()
          var Text = current
          current = resTree.pop()
          current = resTree.pop()
          current = resTree.pop()
          var Link = current
          printTree = "</a>\n" :: Text :: "\">" :: Link :: "<a href=\"" :: printTree
          current = resTree.pop()
          current = resTree.pop()
        }
        case CONSTANTS.IMAGEB => {
          current = resTree.pop()
          var Text = current
          current = resTree.pop()
          current = resTree.pop()
          current = resTree.pop()
          var Link = current
          printTree = "\">" :: Text :: " alt=\"" :: "\"" :: Link :: "<img src=\"" :: printTree
          current = resTree.pop()
          current = resTree.pop()

        }
        case CONSTANTS.DEFB => {
          varStack.push(scope)
          current = resTree.pop()
          varStack.push(current.trim())
          current = resTree.pop()
          current = resTree.pop()
          varStack.push(current.trim())
          current = resTree.pop()
          current = resTree.pop()
        }
        case CONSTANTS.USEB => {
          booleanLi = true
          var resVarStack = varStack.clone()
          var foundBool = false
          var varInfo = resVarStack.pop()
          var varName = resVarStack.pop()
          var varScope = resVarStack.pop()
          current = resTree.pop()
          while(!scope.equalsIgnoreCase(varScope) && !resVarStack.isEmpty){
            varInfo = resVarStack.pop()
            varName = resVarStack.pop()
            varScope = resVarStack.pop()
          }
          if (current.trim().equalsIgnoreCase(varName.trim())) {
            foundBool = true
            printTree = " "::varInfo :: printTree
            resVarStack = varStack.clone()
          } else {
            while (!resVarStack.isEmpty && !current.trim().equalsIgnoreCase(varName.trim())) {
              varInfo = resVarStack.pop()
              varName = resVarStack.pop()
              varScope = resVarStack.pop()
            }
            if (current.trim().equalsIgnoreCase(varName.trim())) {
              foundBool = true
              printTree = " " :: varInfo :: printTree
            }
          }
          if (!foundBool) {
            println("SEMANTIC ERROR: " + current + " not Found!");
            System.exit(1)
          }
          current = resTree.pop()
          current = resTree.pop()
        }
        case _ => {
          printTree = current:: printTree
          current = resTree.pop()
          if(booleanLi)
            liBool = false
          if(liBool){
            printTree = "</li>\n"::printTree
            liBool = false
          }
        }
      }
    }
    if(current.equalsIgnoreCase(CONSTANTS.DOCE)) {
      printTree = "\n</body>\n</html>"::printTree;
    }
    printTree = printTree.reverse
    writeHTML(printTree.mkString)
    openHTMLFileInBrowser(Compiler.filename+ ".html")

  }
  def writeHTML(s : String): Unit = {
    val writer = new PrintWriter(new File(Compiler.filename+ ".html"))
    writer.write(s)
    writer.close()
  }

  /* * Hack Scala/Java function to take a String filename and open in default web browser. */
  def openHTMLFileInBrowser(htmlFileStr : String) = {
    val file : File = new File(htmlFileStr.trim)
    println(file.getAbsolutePath)
    if (!file.exists())
      sys.error("File " + htmlFileStr + " does not exist.")

    try {
      Desktop.getDesktop.browse(file.toURI)
    }
    catch {
      case ioe: IOException => sys.error("Failed to open file:  " + htmlFileStr)
      case e: Exception => sys.error("He's dead, Jim!")
    }
  }
}