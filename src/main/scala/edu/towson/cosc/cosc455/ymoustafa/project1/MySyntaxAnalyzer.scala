package edu.towson.cosc.cosc455.ymoustafa.project1

import scala.collection.mutable.Stack

class MySyntaxAnalyzer extends SyntaxAnalyzer{
  var synParser = new Stack[String]

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
      else {
        println("SYNTAX ERROR: Expected: '" + Tokens.DOCE + "'. Received: '" + MyCompiler.currentToken + "'")
        System.exit(1)
      }
    }
    else {
      println("SYNTAX ERROR: Expected '" + Tokens.DOCB + "'. Received '" + MyCompiler.currentToken + "'")
      System.exit(1)
    }
  }

  def title(): Unit = {
    if (MyCompiler.currentToken.equalsIgnoreCase(Tokens.TITLEB)) {
      synParser.push(MyCompiler.currentToken)
      MyCompiler.Scanner.getNextToken()
      if (MyCompiler.currentToken.equalsIgnoreCase(Tokens.BRACKETE)) {
        synParser.push(MyCompiler.currentToken)
        MyCompiler.Scanner.getNextToken()
      }
      else {
        println("SYNTAX ERROR: Expected '" + Tokens.BRACKETE + "'. Received '" + MyCompiler.currentToken + "'")
        System.exit(1)
      }
    }
    else {
      println("SYNTAX ERROR: Expected: '" + Tokens.TITLEB + "'. Received: '" + MyCompiler.currentToken + "'")
      System.exit(1)
    }
  }

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
      body()
    }
  }

  def paragraph(): Unit = {
    if (MyCompiler.currentToken.equalsIgnoreCase(Tokens.PARAB)) {
      synParser.push(MyCompiler.currentToken)
      MyCompiler.Scanner.getNextToken()
      if (MyCompiler.currentToken.equalsIgnoreCase(Tokens.DEFB)) {
        variableDefine()
      }

      if (MyCompiler.currentToken.equalsIgnoreCase(Tokens.PARAE)) {
        synParser.push(MyCompiler.currentToken)
        MyCompiler.Scanner.getNextToken()
      }
      else {
        println("SYNTAX ERROR: Expected: '" + Tokens.PARAE + "'. Received: '" + MyCompiler.currentToken + "'")
        System.exit(1)
      }
    }
    else {
      println("SYNTAX ERROR: Expected: '" + Tokens.PARAB + "'. Received: '" + MyCompiler.currentToken + "'")
      System.exit(1)
    }
  }

  def heading(): Unit = {
    if (MyCompiler.currentToken.equalsIgnoreCase(Tokens.HEADING)) {
      synParser.push(MyCompiler.currentToken)
      MyCompiler.Scanner.getNextToken()
    }
  }

  def variableDefine(): Unit = {
    if (MyCompiler.currentToken.equalsIgnoreCase(Tokens.DEFB)) {
      synParser.push(MyCompiler.currentToken)
      MyCompiler.Scanner.getNextToken()
      if (MyCompiler.currentToken.equalsIgnoreCase(Tokens.EQSIGN)) {
        synParser.push(MyCompiler.currentToken)
        MyCompiler.Scanner.getNextToken()
        if (MyCompiler.currentToken.equalsIgnoreCase(Tokens.BRACKETE)) {
          synParser.push(MyCompiler.currentToken)
          MyCompiler.Scanner.getNextToken()
          variableDefine()
        }
        else {
          println("SYNTAX ERROR: Expected: '" + Tokens.BRACKETE + "'. Received: '" + MyCompiler.currentToken + "'")
          System.exit(1)
        }
      }
      else {
        println("SYNTAX ERROR: Expected: '" + Tokens.EQSIGN + "'. Received: '" + MyCompiler.currentToken + "'")
        System.exit(1)
      }
    }
  }

  def variableUse(): Unit = {
    if (MyCompiler.currentToken.equalsIgnoreCase(Tokens.USEB)) {
      synParser.push(MyCompiler.currentToken)
      MyCompiler.Scanner.getNextToken()
      if(MyCompiler.currentToken.equalsIgnoreCase(Tokens.BRACKETE)) {
        synParser.push(MyCompiler.currentToken)
        MyCompiler.Scanner.getNextToken()
      }
      else {
        println("SYNTAX ERROR: Expected: '" + Tokens.BRACKETE + "'. Received: '" + MyCompiler.currentToken + "'")
        System.exit(1)
      }
    }
  }

  def bold(): Unit = {
    if (MyCompiler.currentToken.equalsIgnoreCase(Tokens.BOLD)) {
      synParser.push(MyCompiler.currentToken)
      MyCompiler.Scanner.getNextToken()
      if (MyCompiler.currentToken.equalsIgnoreCase(Tokens.BOLD)) {
        synParser.push(MyCompiler.currentToken)
        MyCompiler.Scanner.getNextToken()
      }
      else {
        println("SYNTAX ERROR: Expected: '" + Tokens.BOLD + "'. Received: '" + MyCompiler.currentToken + "'")
        System.exit(1)
      }
    }
  }

  def listItem(): Unit = {
    if (MyCompiler.currentToken.equalsIgnoreCase(Tokens.LISTITEM)) {
      synParser.push(MyCompiler.currentToken)
      MyCompiler.Scanner.getNextToken()
    }
  }

  def link(): Unit = {
    if (MyCompiler.currentToken.equalsIgnoreCase(Tokens.LINKB)) {
      synParser.push(MyCompiler.currentToken)
      MyCompiler.Scanner.getNextToken()
      if (MyCompiler.currentToken.equalsIgnoreCase(Tokens.BRACKETE)) {
        synParser.push(MyCompiler.currentToken)
        MyCompiler.Scanner.getNextToken()
        if (MyCompiler.currentToken.equalsIgnoreCase(Tokens.ADDRESSB)) {
          synParser.push(MyCompiler.currentToken)
          MyCompiler.Scanner.getNextToken()
          if (MyCompiler.currentToken.equalsIgnoreCase(Tokens.ADDRESSE)) {
            synParser.push(MyCompiler.currentToken)
            MyCompiler.Scanner.getNextToken()
          }
          else {
            println("SYNTAX ERROR: Expected: '" + Tokens.ADDRESSE + "'. Received: '" + MyCompiler.currentToken + "'")
            System.exit(1)
          }
        }
        else {
          println("SYNTAX ERROR: Expected: '" + Tokens.ADDRESSB + "'. Received: '" + MyCompiler.currentToken + "'")
          System.exit(1)
        }
      }
      else {
        println("SYNTAX ERROR: Expected: '" + Tokens.BRACKETE + "'. Received: '" + MyCompiler.currentToken + "'")
        System.exit(1)
      }
    }
  }

  def image(): Unit = {
    if (MyCompiler.currentToken.equalsIgnoreCase(Tokens.IMAGEB)) {
      synParser.push(MyCompiler.currentToken)
      MyCompiler.Scanner.getNextToken()
      if (MyCompiler.currentToken.equalsIgnoreCase(Tokens.BRACKETE)) {
        synParser.push(MyCompiler.currentToken)
        MyCompiler.Scanner.getNextToken()
        if (MyCompiler.currentToken.equalsIgnoreCase(Tokens.ADDRESSB)) {
          synParser.push(MyCompiler.currentToken)
          MyCompiler.Scanner.getNextToken()
          if (MyCompiler.currentToken.equalsIgnoreCase(Tokens.ADDRESSE)) {
            synParser.push(MyCompiler.currentToken)
            MyCompiler.Scanner.getNextToken()
          }
          else {
            println("SYNTAX ERROR: Expected: '" + Tokens.ADDRESSE + "'. Received: '" + MyCompiler.currentToken + "'")
            System.exit(1)
          }
        }
        else {
          println("SYNTAX ERROR: Expected: '" + Tokens.ADDRESSB + "'. Received: '" + MyCompiler.currentToken + "'")
          System.exit(1)
        }
      }
      else {
        println("SYNTAX ERROR: Expected: '" + Tokens.BRACKETE + "'. Received: '" + MyCompiler.currentToken + "'")
        System.exit(1)
      }
    }
  }

  def newline(): Unit = {
    if (MyCompiler.currentToken.equalsIgnoreCase(Tokens.NEWLINE)) {
      synParser.push(MyCompiler.currentToken)
      MyCompiler.Scanner.getNextToken()
    }
  }
}
