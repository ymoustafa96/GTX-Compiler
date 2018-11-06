package edu.towson.cosc.cosc455.ymoustafa.project1

import java.io._
import java.awt.Desktop
import java.io.{File, IOException}

import scala.collection.mutable.Stack

class MySemanticAnalyzer {
  // variable declarations
  var output : String = ""
  var stack = new Stack[String]
  var semParser = new Stack[String]
  var varName = new Stack[String]
  var varDef = new Stack[String]
  var parseVarName = new Stack[String]
  var parseVarDef = new Stack[String]
  var token : String = ""
  var ifParse = 0

  // Converts Test Case to an executable HTML file
  def toHTML() =
  {
    semParser = MyCompiler.Parser.synParser.reverse
    token = semParser.pop()
    while (semParser.nonEmpty)
    {
      // takes DOCB and creates <html> in html file
      if (token.equalsIgnoreCase(Tokens.DOCB)) {
        stack.push("<html>\n")
        token = semParser.pop()
      }
      // takes TITLEB and following text and creates <head>, <title>, </title>, and <head> in html file
      else if (token.equalsIgnoreCase(Tokens.TITLEB)) {
        stack.push("<head>\n")
        stack.push("<title>\n")
        token = semParser.pop()
        // keeps popping text from stack until a Token is found
        while (!Tokens.KEYWORDS.contains(token)) {
          stack.push(token + " ")
          token = semParser.pop()
        }
        stack.push("</title>\n")
        stack.push("</head>\n")
        token = semParser.pop()
        // The DEFB within TITLE is set to varName and the following defined variable to varDef
        while (token == Tokens.DEFB)
        {
          if (token.equalsIgnoreCase(Tokens.DEFB)) {
            varName.push(semParser.pop())
            semParser.pop()
            varDef.push(semParser.pop())
            semParser.pop()
            token = semParser.pop()
          }
        }
      }
      // when token is HEADING, it will take the following text as the heading and put it within <h1> and </h1>
      else if (token.equalsIgnoreCase(Tokens.HEADING)) {
        stack.push("<h1>")
        stack.push(semParser.pop()+" ")
        token = semParser.pop()
        while (!Tokens.KEYWORDS.contains(token)) {
          stack.push(token + " ")
          token = semParser.pop()
        }
        stack.push("</h1>\n")
      }
      // when token is PARAB, it will take the following text as the paragraph and put it after <p>
      else if (token.equalsIgnoreCase(Tokens.PARAB)) {
        stack.push("<p>")
        token = semParser.pop()
        ifParse = 1
      }
      // when token is PARAE, it will stop setting text to paragraph and close it with </p>
      else if (token.equalsIgnoreCase(Tokens.PARAE)) {
        stack.push("</p>\n")
        token = semParser.pop()
        ifParse = 0
      }
      // when token is BOLD, it will take the following text as bold text within <b> and </b>
      else if (token.equalsIgnoreCase(Tokens.BOLD)) {
        stack.push("<b>")
        token = semParser.pop()
        while (!Tokens.KEYWORDS.contains(token)) {
          stack.push(token+" ")
          token = semParser.pop()
        }
        stack.push("</b>\n")
        token = semParser.pop()
      }
      // when token is LISTITEM, it will take the following text as a list and put it within <li> and </li>
      else if (token.equalsIgnoreCase(Tokens.LISTITEM)) {
        stack.push("<li>")
        token = semParser.pop()
        while (!Tokens.KEYWORDS.contains(token) && !Tokens.SPECIALCHAR.contains(token)) {
          stack.push(token+" ")
          token = semParser.pop()
        }
        // checks if USEB variable matches the DEFB variable and if it is, it will print it, otherwise throw an error
        while(token.equalsIgnoreCase(Tokens.USEB)) {
          var name: String = semParser.pop()
          semParser.pop()
          if (ifParse == 0) {
            if (varName.contains(name)) {
              stack.push(" " + varDef(varName.indexOf(name, 0)) + " ")
            }
            else {
              // if an undefined variable is used, it will throw a Static Semantic Error
              println("STATIC SEMANTIC ERROR: The variable: [" + name + "] has not been defined")
              System.exit(1)
            }
          }
          else
          {
            if (parseVarName.contains(name)) {
              stack.push(" " + parseVarDef(parseVarName.indexOf(name, 0)) + " ")
            }
            else if (varName.contains(name)) {
              stack.push(" " + varDef(varName.indexOf(name, 0)) + " ")
            }
            // if an undefined variable is used, it will throw a Static Semantic Error
            else if (!parseVarName.contains(name) || !varName.contains(name)) {
              println("STATIC SEMANTIC ERROR: The variable: [" + name + "] has not been defined")
              System.exit(1)
            }
          }
          token = semParser.pop()
        }
        stack.push("</li>")
      }
      // when token is NEWLINE, it will create a <br>
      else if (token.equalsIgnoreCase(Tokens.NEWLINE)) {
        stack.push("<br>\n")
        token = semParser.pop()
      }
      // when token is LINKB, the following text is created as a link, with <a href = "Placeholder">WEBURL</a>
      else if (token.equalsIgnoreCase(Tokens.LINKB)) {
        var temp: String = ""
        token = semParser.pop()
        while (!Tokens.KEYWORDS.contains(token)) {
          temp += token + " "
          token = semParser.pop()
        }
        semParser.pop()
        stack.push("<a href = \"")
        // here the text is Placeholder
        stack.push(semParser.pop())
        stack.push("\">")
        stack.push(temp)
        // here temp is the WEBURL
        stack.push("</a> ")
        semParser.pop()
        token = semParser.pop()
      }
      // when token is IMAGEB, the following text is created as an image link, with <img src = "IMGURL" alt="IMG_Name">
      else if (token.equalsIgnoreCase(Tokens.IMAGEB)) {
        var temp: String = ""
        token = semParser.pop()
        while (!Tokens.KEYWORDS.contains(token)) {
          temp += token + " "
          token = semParser.pop()
        }
        semParser.pop()
        token = semParser.pop()

        stack.push("<img src =\"")
        // here is the IMGURL
        stack.push(token)
        stack.push("\" alt=\"")
        // here is the IMG_Name
        stack.push(temp)
        stack.push("\">\n")
        semParser.pop()
        token = semParser.pop()
      }
      // The DEFB within PARAGRAPH is set to varName and the following defined variable to varDef
      else if (token.equalsIgnoreCase(Tokens.DEFB)) {
        if (ifParse == 0) {
          varName.push(semParser.pop())
          semParser.pop()
          varDef.push(semParser.pop())
          semParser.pop()
          token = semParser.pop()
        }
        else {
          parseVarName.push(semParser.pop())
          semParser.pop()
          parseVarDef.push(semParser.pop())
          semParser.pop()
          token = semParser.pop()
        }
      }
      // The USEB within PARAGRAPH is checked again to see if it matches the DEFB
      else if (token.equalsIgnoreCase(Tokens.USEB)) {
        var name: String = semParser.pop()
        semParser.pop()
        if (ifParse == 0) {
          if (varName.contains(name)) {
            stack.push(" " + varDef(varName.indexOf(name, 0)) + " ")
          }
          else {
            // if an undefined variable is used, it will throw a Static Semantic Error
            println("STATIC SEMANTIC ERROR: The variable: [" + name + "] has not been defined")
            System.exit(1)
          }
        }
        else
        {
          if (parseVarName.contains(name)) {
            stack.push(" " + parseVarDef(parseVarName.indexOf(name, 0)) + " ")
          }
          else if (varName.contains(name)) {
            stack.push(" " + varDef(varName.indexOf(name, 0)) + " ")
          }
          else if (!parseVarName.contains(name) || !varName.contains(name)) {
            // if an undefined variable is used, it will throw a Static Semantic Error
            println("STATIC SEMANTIC ERROR: The variable: [" + name + "] has not been defined")
            System.exit(1)
          }
        }
        token = semParser.pop()
      }
      else if (token.equalsIgnoreCase(Tokens.DOCE)) {
        stack.push("</html>\n")
      }
      else if (!Tokens.KEYWORDS.contains(token)) {
        stack.push(token+" ")
        token = semParser.pop()
      }
    }
    // turns complete stack to string for output and prints that output into an html file
    val output = stack.reverse.mkString
    val print = new PrintWriter(new File(MyCompiler.filename + ".html"))
    print.write(output)
    print.close
    openHTMLFileInBrowser(MyCompiler.filename + ".html")
  }

  // Takes a String filename and open in default web browser.
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