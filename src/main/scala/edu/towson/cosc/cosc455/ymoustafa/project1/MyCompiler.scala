package edu.towson.cosc.cosc455.ymoustafa.project1

object MyCompiler {
  // check args *
  // check file extension
  // initializations
  // get first token
  // call start state

  var currentToken : String = "" // bin to put token in
  var fileContents : String = ""
  var filename : String = "" // takes input for filename

  val Scanner = new MyLexicalAnalyzer
  val Parser = new MySyntaxAnalyzer
  val SemanticAnalyzer = new MySemanticAnalyzer
  var position : Int = -1

  def main(args: Array[String]): Unit = {
    checkFile(args)
    filename = args(0)
    readFile(args(0))

    Scanner.getNextToken() // calls getNextToken to tokenize file
    Parser.gittex() // calls start state

    // Prints test cases in terminal
    // print(fileContents)

    // Calls toHTML() function in MySemanticAnalyzer to create and run HTML file
    SemanticAnalyzer.toHTML()
  }

  // reads input file
  def readFile(file : String) = {
    val source = scala.io.Source.fromFile(file)
    fileContents = try source.mkString finally source.close()
  }

  // checks file args
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
