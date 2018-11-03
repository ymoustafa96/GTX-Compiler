package edu.towson.cosc.cosc455.ymoustafa.project1

trait LexicalAnalyzer {
  def addChar() : Unit
  def getChar() : Unit
  def getNextToken() : Unit
  def lookup() : Boolean
}