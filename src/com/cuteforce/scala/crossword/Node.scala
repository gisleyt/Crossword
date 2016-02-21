package com.cuteforce.scala.crossword

import scala.annotation.tailrec
import scala.collection.mutable

class Node(val letter : Char) {
   val daughters = mutable.Map[Char, Node]() 
   
   def this() {
      this('%')
   }

  @tailrec final def getNode(prefix : Array[Char]) : Option[Node] = {
    val nodeOption = this.daughters.get(prefix.head)
     if (!nodeOption.isDefined || prefix.length == 1) {
       return nodeOption
     } else {
       return nodeOption.get.getNode(prefix.tail)
     }
  }
}
