package edu.towson.cosc.cosc455.ymoustafa.project1

import scala.collection.mutable.Stack

class MySyntaxAnalyzer extends SyntaxAnalyzer{
  // initialize synParser
  var synParser = new Stack[String]

  // start state
  def gittex(): Unit = {
    if (MyCompiler.currentToken.equalsIgnoreCase(Tokens.DOCB)) {
      synParser.push(MyCompiler.currentToken)
      MyCompiler.Scanner.getNextToken()
      variableDefine()
      title()
      body()
      if(MyCompiler.currentToken.equalsIgnoreCase(Tokens.DOCE)) {
        synParser.push(MyCompiler.currentToken)
      }
      // if the wrong token is used (not DOCE), it will throw a Syntax Error
      else {
        println("SYNTAX ERROR: Expected: '" + Tokens.DOCE + "'. Received: '" + MyCompiler.currentToken + "'")
        System.exit(1)
      }
    }
    // if the wrong token is used (not DOCB), it will throw a Syntax Error
    else {
      println("SYNTAX ERROR: Expected '" + Tokens.DOCB + "'. Received '" + MyCompiler.currentToken + "'")
      System.exit(1)
    }
  }

  // checks for correct input and structure of the title
  def title(): Unit = {
    if (MyCompiler.currentToken.equalsIgnoreCase(Tokens.TITLEB)) {
      synParser.push(MyCompiler.currentToken)
      MyCompiler.Scanner.getNextToken()
      //calls for the regular text check
      regularText()
      if (MyCompiler.currentToken.equalsIgnoreCase(Tokens.BRACKETE)) {
        synParser.push(MyCompiler.currentToken)
        MyCompiler.Scanner.getNextToken()
      }
      // if the wrong token is used (not BRACKETE), it will throw a Syntax Error
      else {
        println("SYNTAX ERROR: Expected '" + Tokens.BRACKETE + "'. Received '" + MyCompiler.currentToken + "'")
        System.exit(1)
      }
    }
    // if the wrong token is used (not TITLEB), it will throw a Syntax Error
    else {
      println("SYNTAX ERROR: Expected: '" + Tokens.TITLEB + "'. Received: '" + MyCompiler.currentToken + "'")
      System.exit(1)
    }
  }

  // checks for correct input and structure of the body
  def body(): Unit = {
    if (MyCompiler.position == MyCompiler.Scanner.fileLength) {
      // Do nothing
    }
    else if (MyCompiler.currentToken.equalsIgnoreCase(Tokens.PARAB)
      || MyCompiler.currentToken.equalsIgnoreCase(Tokens.PARAE)) {
      paragraph()
      body()
    }
    else if (MyCompiler.currentToken.equalsIgnoreCase(Tokens.NEWLINE)) {
      newline()
      body()
    }
    else {
      innerText()
      body()
    }
  }

  // checks for correct input and structure of the paragraph
  def paragraph(): Unit = {
    if (MyCompiler.currentToken.equalsIgnoreCase(Tokens.PARAB)) {
      synParser.push(MyCompiler.currentToken)
      MyCompiler.Scanner.getNextToken()
      if (MyCompiler.currentToken.equalsIgnoreCase(Tokens.DEFB)) {
        variableDefine()
      }
      innerText()

      if (MyCompiler.currentToken.equalsIgnoreCase(Tokens.PARAE)) {
        synParser.push(MyCompiler.currentToken)
        MyCompiler.Scanner.getNextToken()
      }
      // if the wrong token is used (not PARAE), it will throw a Syntax Error
      else {
        println("SYNTAX ERROR: Expected: '" + Tokens.PARAE + "'. Received: '" + MyCompiler.currentToken + "'")
        System.exit(1)
      }
    }
    // if the wrong token is used (not PARAB), it will throw a Syntax Error
    else {
      println("SYNTAX ERROR: Expected: '" + Tokens.PARAB + "'. Received: '" + MyCompiler.currentToken + "'")
      System.exit(1)
    }
  }

  // checks for correct input and structure of the heading
  def heading(): Unit = {
    if (MyCompiler.currentToken.equalsIgnoreCase(Tokens.HEADING)) {
      synParser.push(MyCompiler.currentToken)
      MyCompiler.Scanner.getNextToken()
      regularText()
    }
  }

  // checks for correct input and structure of defined variable
  def variableDefine(): Unit = {
    if (MyCompiler.currentToken.equalsIgnoreCase(Tokens.DEFB)) {
      synParser.push(MyCompiler.currentToken)
      MyCompiler.Scanner.getNextToken()
      regularText()
      if (MyCompiler.currentToken.equalsIgnoreCase(Tokens.EQSIGN)) {
        synParser.push(MyCompiler.currentToken)
        MyCompiler.Scanner.getNextToken()
        regularText()
        if (MyCompiler.currentToken.equalsIgnoreCase(Tokens.BRACKETE)) {
          synParser.push(MyCompiler.currentToken)
          MyCompiler.Scanner.getNextToken()
          variableDefine()
        }
        // if the wrong token is used (not BRACKETE), it will throw a Syntax Error
        else {
          println("SYNTAX ERROR: Expected: '" + Tokens.BRACKETE + "'. Received: '" + MyCompiler.currentToken + "'")
          System.exit(1)
        }
      }
      // if the wrong token is used (not EQSIGN), it will throw a Syntax Error
      else {
        println("SYNTAX ERROR: Expected: '" + Tokens.EQSIGN + "'. Received: '" + MyCompiler.currentToken + "'")
        System.exit(1)
      }
    }
  }

  // checks for correct input and structure of used variable
  def variableUse(): Unit = {
    if (MyCompiler.currentToken.equalsIgnoreCase(Tokens.USEB)) {
      synParser.push(MyCompiler.currentToken)
      MyCompiler.Scanner.getNextToken()
      regularText()
      if(MyCompiler.currentToken.equalsIgnoreCase(Tokens.BRACKETE)) {
        synParser.push(MyCompiler.currentToken)
        MyCompiler.Scanner.getNextToken()
      }
      // if the wrong token is used (not BRACKETE), it will throw a Syntax Error
      else {
        println("SYNTAX ERROR: Expected: '" + Tokens.BRACKETE + "'. Received: '" + MyCompiler.currentToken + "'")
        System.exit(1)
      }
    }
  }

  // checks for correct input and structure of bold text
  def bold(): Unit = {
    if (MyCompiler.currentToken.equalsIgnoreCase(Tokens.BOLD)) {
      synParser.push(MyCompiler.currentToken)
      MyCompiler.Scanner.getNextToken()
      regularText()
      if (MyCompiler.currentToken.equalsIgnoreCase(Tokens.BOLD)) {
        synParser.push(MyCompiler.currentToken)
        MyCompiler.Scanner.getNextToken()
      }
      // if the wrong token is used (not BOLD), it will throw a Syntax Error
      else {
        println("SYNTAX ERROR: Expected: '" + Tokens.BOLD + "'. Received: '" + MyCompiler.currentToken + "'")
        System.exit(1)
      }
    }
  }

  // checks for correct input and structure of list items
  def listItem(): Unit = {
    if (MyCompiler.currentToken.equalsIgnoreCase(Tokens.LISTITEM)) {
      synParser.push(MyCompiler.currentToken)
      MyCompiler.Scanner.getNextToken()
    }
  }

  // checks for correct input and structure of links
  def link(): Unit = {
    if (MyCompiler.currentToken.equalsIgnoreCase(Tokens.LINKB)) {
      synParser.push(MyCompiler.currentToken)
      MyCompiler.Scanner.getNextToken()
      regularText()
      if (MyCompiler.currentToken.equalsIgnoreCase(Tokens.BRACKETE)) {
        synParser.push(MyCompiler.currentToken)
        MyCompiler.Scanner.getNextToken()
        if (MyCompiler.currentToken.equalsIgnoreCase(Tokens.ADDRESSB)) {
          synParser.push(MyCompiler.currentToken)
          MyCompiler.Scanner.getNextToken()
          regularText()
          if (MyCompiler.currentToken.equalsIgnoreCase(Tokens.ADDRESSE)) {
            synParser.push(MyCompiler.currentToken)
            MyCompiler.Scanner.getNextToken()
          }
          // if the wrong token is used (not ADDRESSE), it will throw a Syntax Error
          else {
            println("SYNTAX ERROR: Expected: '" + Tokens.ADDRESSE + "'. Received: '" + MyCompiler.currentToken + "'")
            System.exit(1)
          }
        }
        // if the wrong token is used (not ADDRESSB), it will throw a Syntax Error
        else {
          println("SYNTAX ERROR: Expected: '" + Tokens.ADDRESSB + "'. Received: '" + MyCompiler.currentToken + "'")
          System.exit(1)
        }
      }
      // if the wrong token is used (not BRACKETE), it will throw a Syntax Error
      else {
        println("SYNTAX ERROR: Expected: '" + Tokens.BRACKETE + "'. Received: '" + MyCompiler.currentToken + "'")
        System.exit(1)
      }
    }
  }

  // checks for correct input and structure of images
  def image(): Unit = {
    if (MyCompiler.currentToken.equalsIgnoreCase(Tokens.IMAGEB)) {
      synParser.push(MyCompiler.currentToken)
      MyCompiler.Scanner.getNextToken()
      regularText()
      if (MyCompiler.currentToken.equalsIgnoreCase(Tokens.BRACKETE)) {
        synParser.push(MyCompiler.currentToken)
        MyCompiler.Scanner.getNextToken()
        if (MyCompiler.currentToken.equalsIgnoreCase(Tokens.ADDRESSB)) {
          synParser.push(MyCompiler.currentToken)
          MyCompiler.Scanner.getNextToken()
          regularText()
          if (MyCompiler.currentToken.equalsIgnoreCase(Tokens.ADDRESSE)) {
            synParser.push(MyCompiler.currentToken)
            MyCompiler.Scanner.getNextToken()
          }
          // if the wrong token is used (not ADDRESSE), it will throw a Syntax Error
          else {
            println("SYNTAX ERROR: Expected: '" + Tokens.ADDRESSE + "'. Received: '" + MyCompiler.currentToken + "'")
            System.exit(1)
          }
        }
        // if the wrong token is used (not ADDRESSB), it will throw a Syntax Error
        else {
          println("SYNTAX ERROR: Expected: '" + Tokens.ADDRESSB + "'. Received: '" + MyCompiler.currentToken + "'")
          System.exit(1)
        }
      }
      // if the wrong token is used (not BRACKETE), it will throw a Syntax Error
      else {
        println("SYNTAX ERROR: Expected: '" + Tokens.BRACKETE + "'. Received: '" + MyCompiler.currentToken + "'")
        System.exit(1)
      }
    }
  }

  // checks for correct input and structure of a new line
  def newline(): Unit = {
    if (MyCompiler.currentToken.equalsIgnoreCase(Tokens.NEWLINE)) {
      synParser.push(MyCompiler.currentToken)
      MyCompiler.Scanner.getNextToken()
    }
  }

  // checks through the text within each section
  def innerText(): Unit = {
    if (MyCompiler.currentToken.equalsIgnoreCase(Tokens.USEB)) {
      variableUse()
      innerText()
    }
    else if (MyCompiler.currentToken.equalsIgnoreCase(Tokens.HEADING)) {
      heading()
      innerText()
    }
    else if (MyCompiler.currentToken.equalsIgnoreCase(Tokens.BOLD)) {
      bold()
      innerText()
    }
    else if (MyCompiler.currentToken.equalsIgnoreCase(Tokens.LISTITEM)) {
      listItem()
      innerText()
    }
    else if (MyCompiler.currentToken.equalsIgnoreCase(Tokens.IMAGEB)) {
      image()
      innerText()
    }
    else if (MyCompiler.currentToken.equalsIgnoreCase(Tokens.LINKB)) {
      link()
      innerText()
    }
    else if (MyCompiler.currentToken.equalsIgnoreCase(Tokens.NEWLINE)) {
      newline()
      innerText()
    }
    else if (textCheck()) {
      synParser.push(MyCompiler.currentToken)
      MyCompiler.Scanner.getNextToken()
      innerText()
    }
  }

  // Added the regular text needed for title, body, paragraph, etc.
  def regularText(): Unit = {
    if (textCheck()) {
      synParser.push(MyCompiler.currentToken)
      MyCompiler.Scanner.getNextToken()
      regularText()
    }
    else if (MyCompiler.position == MyCompiler.Scanner.fileLength) {
      // Do nothing
    }
  }

  // checks form of text
  def textCheck(): Boolean = {
    if (MyCompiler.currentToken.contains(':')
      || MyCompiler.currentToken.contains('.')
      || MyCompiler.currentToken.contains(',')) {
      return true
    }
    if (MyCompiler.currentToken.contains("\n")) {
      return MyCompiler.currentToken.length == MyCompiler.currentToken.filter(_.isLetterOrDigit).length + 1
    }
    MyCompiler.currentToken.length == MyCompiler.currentToken.filter(_.isLetterOrDigit).length
  }
}
