package com.cuteforce.scala.crossword

object Trie {
  
  def inject(node: Node, word : Array[Char]): Unit = {
    if (!word.isEmpty) {
      if (!node.daughters.contains(word.head)) {
        node.daughters.put(word.head, new Node(word.head))
      }
      inject(node.daughters(word.head), word.tail)
    } else {
      node.daughters.put('.', new Node())
    }
  }
  
  def contains(node: Node, word : Array[Char]): Boolean = {
    if (word.isEmpty)
      node.daughters.contains('.')
    else {
      val daughter = node.daughters.get(word.head)
      daughter match {
        case Some(node) => contains(node, word.tail)
        case None => false
      }
    }
  }
  
  def getDictionary(): Node = {
    new Node()
  }
}
