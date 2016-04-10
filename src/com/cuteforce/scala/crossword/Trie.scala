package com.cuteforce.scala.crossword

import scala.annotation.tailrec


object Trie {

  @tailrec final def inject(node: Node, word : Array[Char]): Unit = {
    if (!word.isEmpty) {
      if (!node.daughters.contains(word.head)) {
        node.daughters.put(word.head, new Node(Some(word.head)))
      }
      inject(node.daughters(word.head), word.tail)
    } else {
      node.frequency += 1
    }
  }


  final def getWords(node: Node, prefix: Array[Char]): Stream[String] = {
    if (node.letter.isEmpty) {
      return node.daughters.valuesIterator.flatMap(node => getWords(node, prefix)).toStream
    } else {
      val newPrefix = Array.concat(prefix, Array(node.letter.get))
      if (node.frequency > 0 && node.letter.isDefined) {
        return Stream.cons(newPrefix.mkString, node.daughters.valuesIterator.flatMap(node => getWords(node, newPrefix)).toStream)
      } else {
        return node.daughters.valuesIterator.flatMap(node => getWords(node, newPrefix)).toStream
      }
    }
  }


  final def getWordFrequencies(node: Node, prefix: Array[Char]): Stream[Tuple2[String, Integer]] = {
    if (node.letter.isEmpty) {
      return node.daughters.valuesIterator.flatMap(node => getWordFrequencies(node, prefix)).toStream
    } else {
      val newPrefix = Array.concat(prefix, Array(node.letter.get))
      if (node.frequency > 0 && node.letter.isDefined) {
        return Stream.cons(new Tuple2(newPrefix.mkString, node.frequency), node.daughters.valuesIterator.flatMap(node => getWordFrequencies(node, newPrefix)).toStream)
      } else {
        return node.daughters.valuesIterator.flatMap(node => getWordFrequencies(node, newPrefix)).toStream
      }
    }
  }


  @tailrec final def contains(node: Node, word : Array[Char]): Boolean = {
    if (word.isEmpty)
      node.frequency > 0
    else {
      if (node.daughters.contains(word.head)) contains(node, word.tail) else false
    }
  }


  def getDictionary(): Node = {
    new Node()
  }
}
