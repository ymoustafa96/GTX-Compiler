package edu.towson.cosc.cosc455.ymoustafa.project1

object MyCompiler {

  var currentToken : String = ""

  def main(args: Array[String]): Unit = {

    // check if an input file is provided
    if (args.length == 0) {
      //USAGE ERROR
      println("USAGE ERROR: Must provide an input file.")
      System.exit(0)
    }

    if (!checkFilenameExtentsion(args(0))) {
      //USAGE ERROR
      println("USAGE ERROR: Input file must be .gtx.")
      System.exit(0)
    }

    val Scanner = new MyLexicalAnalyzer
    val Parser = new MySyntaxAnalyzer

    setCurrentToken(Scanner.getNextToken())
    Parser.gittex()

    // .....
    // If it gets here, it is compiled
    // post processing

  }

  def checkFilenameExtentsion(filename : String) : Boolean = filename.endsWith(".gtx")

  def getCurrentToken() : String = this.currentToken
  def setCurrentToken(t : String) : Unit = this.currentToken = t
}
