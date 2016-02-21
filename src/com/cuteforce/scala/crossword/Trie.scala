package com.cuteforce.scala.crossword

import scala.annotation.tailrec

object Trie {
  
  @tailrec final def inject(node: Node, word : Array[Char]): Unit = {
    if (!word.isEmpty) {
      if (!node.daughters.contains(word.head)) {
        node.daughters.put(word.head, new Node(word.head))
      }
      inject(node.daughters(word.head), word.tail)
    } else {
      node.daughters.put('.', new Node())
    }
  }
  
  @tailrec final def contains(node: Node, word : Array[Char]): Boolean = {
    if (word.isEmpty)
      node.daughters.contains('.')
    else {
      if (node.daughters.contains(word.head)) contains(node, word.tail) else false
    }
  }
  
  def getDictionary(): Node = {
    new Node()
  }
}
