package edu.towson.cosc.cosc455.ymoustafa.project1

class MyLexicalAnalyzer extends LexicalAnalyzer{
  // variable declarations
  var nextChar : Char = ' '
  var tokenString : String = ""
  var fileLength : Int = 0

  // Adds chars to token string one at a time
  def addChar(): Unit = {
    tokenString += nextChar
  }

  // Gets the next char and puts it in the current position
  def getChar(): Unit = {
    if (MyCompiler.position < fileLength) {
      MyCompiler.position += 1
      nextChar = MyCompiler.fileContents.charAt(MyCompiler.position)
    }
  }

  // Retrieves next token
  def getNextToken(): Unit = {
    fileLength = MyCompiler.fileContents.length - 1
    tokenString = ""

    getChar()
    emptySpace()

    if (fileLength != MyCompiler.position) {
      //do nothing
    }
    // checks for special characters; '*', '+', '=', '\\', '!', '#', '[', ']', '(', ')'
    if (Tokens.SPECIALCHAR.contains(nextChar)) {
      if (Tokens.BOLD.contains(nextChar)) {
        addChar()
        getChar()
      }
      else if (Tokens.LISTITEM.contains(nextChar)) {
        addChar()
        tokenString += readAll()
      }
      else if (Tokens.ADDRESSE.contains(nextChar)) {
        addChar()
        getChar()
      }
      else if (Tokens.SPECIALCHAR(3) == nextChar) {
        addChar()
        tokenString += readAll()
        if (Tokens.SPECIALCHAR(3) == nextChar) {
          addChar()
          MyCompiler.currentToken = tokenString
          MyCompiler.position += 1
          return
        }
        if (Tokens.SPECIALCHAR(6) == nextChar) {
          addChar()
          MyCompiler.currentToken = tokenString
          return
        }
        if (Tokens.BRACKETE.contains(nextChar)) {
          addChar()
        }
        // cannot have text after DOCE token, will throw a Syntax Error
        if (Tokens.DOCE == tokenString.toUpperCase) {
          emptySpace()
          if (MyCompiler.position - fileLength != 0) {
            MyCompiler.position -= 1
            getNextToken()
            println("SYNTAX ERROR: Cannot have any text after \\END")
            System.exit(1)
          }
        }
      }
      // checks for Heading token; '#'
      else if (Tokens.HEADING.contains(nextChar)) {
        addChar()
        tokenString += readAll()
      }
      // checks for IMAGEB token; '!['
      else if (Tokens.IMAGEB.charAt(0) == nextChar) {
        addChar()
        getChar()
        if (Tokens.IMAGEB.charAt(1) == nextChar) {
          addChar()
          if (lookup()) {
            if (tokenString.substring(tokenString.length - 1, tokenString.length) == "\n"
              || tokenString.substring(tokenString.length - 1, tokenString.length) == "\r"
              || tokenString.substring(tokenString.length - 1, tokenString.length) == "\t") {
              MyCompiler.currentToken = tokenString.substring(0, tokenString.length - 1)
              return
            }
            else {
              MyCompiler.currentToken = tokenString
              return
            }
          }
        }
        // throws a Lexical Error if illegal character (not '[') followed after '!' for Heading token
        else {
          println("LEXICAL ERROR: Illegal character after '!'; '" + nextChar + "'")
          System.exit(1)
        }
      }
      else if (Tokens.SPECIALCHAR.contains(nextChar)) {
        addChar()
      }
      if (tokenString.length > 0 && lookup()) {
        if (tokenString.substring(tokenString.length - 1, tokenString.length) == "\n"
          || tokenString.substring(tokenString.length - 1, tokenString.length) == "\r"
          || tokenString.substring(tokenString.length - 1, tokenString.length) == "\t") {
          MyCompiler.currentToken = tokenString.substring(0, tokenString.length - 1)
        }
        else MyCompiler.currentToken = tokenString
      }
      // throws a Lexical Error if illegal token is found
      else {
        println("LEXICAL ERROR: Illegal token: '" + tokenString + "'")
        System.exit(1)
      }
    }
    // checks if nextChar is a letter, number, ':', '.', or ','
    else if (nextChar.isLetterOrDigit ||nextChar == ':' || nextChar == '.' || nextChar == ',') {
      addChar()
      tokenString += readAll()
      if (nextChar.toString.equals(Tokens.ADDRESSE)
        || nextChar.toString.equals(Tokens.BRACKETE)
        || nextChar.toString.equals(Tokens.PARAE)
        || nextChar.toString.equals(Tokens.EQSIGN)
        || nextChar.equals('\\')) {
        // Will decrement index so special characters aren't skipped
        MyCompiler.position -= 1
      }
      MyCompiler.currentToken = tokenString
    }
  }

  // Looks up tokens and stores as temp
  def lookup(): Boolean = {
    var temp: String = ""
    if (tokenString.substring(tokenString.length - 1,tokenString.length) == "\n"
      || tokenString.substring(tokenString.length - 1,tokenString.length) == "\r"
      || tokenString.substring(tokenString.length - 1,tokenString.length) == "\t") {
      temp = tokenString.toUpperCase.substring(0,tokenString.length - 1)
      return Tokens.KEYWORDS.contains(temp)
    }
    else {
      return Tokens.KEYWORDS.contains(tokenString.toUpperCase)
    }
  }

  // Calls getChar() until an empty space Char is found
  def emptySpace() : Unit = {
    while (Tokens.ENDOFLINE.contains(nextChar) && MyCompiler.position < fileLength) {
      getChar()
    }
  }

  // Reads within text until end of word, line, or token
  def readAll() : String = {
    var text: String = ""
    getChar()

    while (MyCompiler.position < fileLength && !Tokens.ENDOFLINE.contains(nextChar) && !Tokens.SPECIALCHAR.contains(nextChar)) {
      text += nextChar
      getChar()
    }
    if (Tokens.ENDOFLINE(0) == nextChar) {
      text += nextChar
    }
    if (Tokens.ENDOFLINE(1) == nextChar) {
      getChar()
      if (Tokens.ENDOFLINE(2) == nextChar) {
        text += nextChar
      }
    }
    return text
  }
}
