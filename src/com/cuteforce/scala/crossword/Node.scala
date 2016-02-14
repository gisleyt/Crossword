package com.cuteforce.scala.crossword

import scala.collection.mutable

class Node(val letter : Char) {
   val daughters = mutable.Map[Char, Node]() 
   
   def this() {
      this('%')
   }
   
    def getNode(prefix : Array[Char]) : Option[Node] = {
      val nodeOption = this.daughters.get(prefix(0))
       if (nodeOption.isDefined) {
        if (prefix.length == 1) nodeOption else nodeOption.get.getNode(prefix.drop(1)) 
      } else {
        nodeOption
      }
    }
}
