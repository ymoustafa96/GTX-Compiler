package edu.towson.cosc.cosc455.ymoustafa.project1

object MyCompiler {

  var currentToken : String = ""
  var fileContents : String = ""
  var filename : String = ""

  val Scanner = new MyLexicalAnalyzer
  val Parser = new MySyntaxAnalyzer
  val SemanticAnalyzer = new MySemanticAnalyzer
  var position : Int = -1

  def main(args: Array[String]): Unit = {
    checkFile(args)
    filename = args(0)
    readFile(args(0))

    Scanner.getNextToken()
    Parser.gittex()

    // Prints test cases in terminal
    print(fileContents)
  }


  def readFile(file : String) = {
    val source = scala.io.Source.fromFile(file)
    fileContents = try source.mkString finally source.close()
  }

  def checkFile(args : Array[String]) = {
    if (args.length == 0) {
      //USAGE ERROR
      println("USAGE ERROR: Must provide an input file.")
      System.exit(1)
    }
    else if (args.length != 1) {
      println("USAGE ERROR: Wrong number of args.")
      System.exit(1)
    }
    else if (!checkFilenameExtension(args(0))) {
      println("USAGE ERROR: Input file must be .gtx.")
      System.exit(1)
    }
  }
  def checkFilenameExtension(filename : String) : Boolean = filename.endsWith(".gtx")
}
