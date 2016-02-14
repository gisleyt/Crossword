package com.cuteforce.scala.crossword

object Trie {
  
  def inject(node: Node, word : Array[Char]): Unit = {
    if (!word.isEmpty) {
      if (!node.daughters.contains(word(0))) {
        node.daughters.put(word(0), new Node(word(0)))
      }
      inject(node.daughters(word(0)), word.tail)
    } else {
      node.daughters.put('.', new Node())
    }
  }
  
  def contains(node: Node, word : Array[Char]): Boolean = {
    if (word.isEmpty)
      node.daughters.contains('.')
    else {
      val daughter = node.daughters.get(word(0))
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
