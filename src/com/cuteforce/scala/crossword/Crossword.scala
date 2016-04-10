package com.cuteforce.scala.crossword

import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}

import scala.collection.GenSeq
import scala.io.Source

class Crossword(val size: Int, dictionary: String) {
  val root = Trie.getDictionary()
  var failedPaths = new AtomicInteger;
  val startTime = System.currentTimeMillis();
  var foundSolution = new AtomicBoolean
  
  Source.fromFile(dictionary).getLines()
    .flatMap(_.split('.'))
    .filter(_.length() == this.size)
    .foreach(x => Trie.inject(this.root, x))

  
  def solve(letters : Array[Char]): Option[Array[Char]] = {
    if (this.foundSolution.get) {
      return None
    }
    if (letters.length < this.size * this.size) {
      val nextLetters: GenSeq[Char] = if (letters.length == 0) {
        getPlausibleNextChars(getHorizontal(letters), getVertical(letters)).toList.par
      } else {
        getPlausibleNextChars(getHorizontal(letters), getVertical(letters)).toList
      }
      val res = nextLetters.map(letter => solve(Array.concat(letters, Array(letter)))).find(_.isDefined)
      if (res.isEmpty) {
        this.failedPaths.incrementAndGet()
        if (this.failedPaths.get % 1000000 == 0) {
          println(this.failedPaths.get)
          println("Paths per sec: " + this.failedPaths.get / ((System.currentTimeMillis().toDouble - this.startTime.toDouble) / 1000.0))
        }
      }
      res.getOrElse(None)
    } else {
      println("FOUND SOLUTION after attempt num " + this.failedPaths.get)
      this.foundSolution.set(true)
      Option(letters)
    }
  }
    
  
  def getPlausibleNextChars(prefix1: Array[Char], prefix2: Array[Char]): Iterator[Char] = {
    val node1 = if (prefix1.length == 0) Some(this.root) else this.root.getNode(prefix1)
    val node2 = if (prefix2.length == 0) Some(this.root) else this.root.getNode(prefix2)
    if (node1.isEmpty || node2.isEmpty) {
      Iterator.empty
    } else {
      node1.get.daughters.keysIterator.filter(x => node2.get.daughters.contains(x))
    }
  }

  def getHorizontal(letters: Array[Char]): Array[Char] = {
    letters.drop(letters.length - letters.length % this.size)
  }

  def getVertical(letters: Array[Char]): Array[Char] = {
    val vertical = new StringBuilder
    var currentIdx = letters.length % this.size
    while (currentIdx < letters.length) {
      vertical.append(letters(currentIdx))
      currentIdx += this.size
    }
    vertical.toArray
  }
}

object Crossword {
  def main(args: Array[String]) {
    val crossword = new Crossword(args(0).toInt, args(1))
    val res = crossword.solve(Array()).getOrElse(Array())
    println(res.length)
    for (i <- 0 to res.length - 1) {
      if (i % args(0).toInt == 0) {
        print('\n')
        print(res(i))
      } else {
        print(res(i))
      }
    }
  }
}
