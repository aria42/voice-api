package com.pragmaticideal.voiceapi.cfg.parser

import com.pragmaticideal.voiceapi.cfg.{State, BinaryGrammar}
import com.pragmaticideal.voiceapi.tree.{Tree, Branch, Leaf}

import scala.collection.mutable

trait Parser {

  def parseStates(states: Seq[State]): Option[(Tree, Double)] = parseLattice(states.map(s => Map(s -> 0.0)))

  def parseLattice(weightedStates: Seq[Map[State, Double]]): Option[(Tree, Double)]

  def parseSentence(sentence: Seq[String]): Option[(Tree, Double)]
}

class AgendaParser(val grammar: BinaryGrammar) extends Parser {
  case class Edge(val tree: Tree, val score: Double) {
    def signature = EdgeSignature(tree.state, span)
    def span = tree.span
    def length = span._2 - span._1
  }

  case class EdgeSignature(val state: State, val span: (Int, Int))

  override def parseSentence(sentence: Seq[String]): Option[(Tree, Double)] = {
    require(grammar.lexicon.isDefined, "Need a lexicon to parse raw sentence")
    parseLattice(grammar.lexicon.get.wordTrellis(sentence))
  }

  override def parseLattice(sentence: Seq[Map[State, Double]]): Option[(Tree, Double)] = {
    val n = sentence.length
    // Agenda is a PQ on edges priortized on span size, then on score
    val edgeOrdering = Ordering.by((e: Edge) => (-e.length, e.score))
    val agenda = mutable.PriorityQueue[Edge]()(edgeOrdering)
    // Chart stores best (first-encountered) derivation of EdgeSignature
    val chart = mutable.Map[EdgeSignature, Edge]()
    // If you discover a new edge, add it to chart/agenda, explore later
    def discoverEdge(edge: Edge) {
      if (!chart.contains(edge.signature)) {
        chart.put(edge.signature, edge)
        agenda += edge
      }
    }
    for (stateMap <- sentence; (state, weight) <- stateMap) {
      require( grammar.unarysByChild.contains(state) ||
        grammar.binarysByLefChild.contains(state) ||
        grammar.binarysByRightmost.contains(state),
        s"State $state not in grammar")
    }
    // base case: discover terminal edges
    val terminalEdges = for {
      (wmap, idx) <- sentence.zipWithIndex
      (s, weight) <- wmap
      leaf = Leaf(s, idx)
    } yield Edge(leaf, weight)
    terminalEdges.foreach(discoverEdge)
    val goalSignature = EdgeSignature(grammar.root, (0, n))
    // finished when we have a goal edge or out of edges
    def isFinished = agenda.headOption.map(_.signature == goalSignature).getOrElse(true)
    while (!isFinished) {
      val e = agenda.dequeue
      //println(agenda)
      // unary expansion bottom-up
      val unaryExpansion = for {
        r <- grammar.unarysByChild.getOrElse(e.tree.state, Seq())
        newTree = Branch(r.parent, Seq(e.tree))
      } yield Edge(newTree, e.score + r.score)
      val (start, stop) = e.span
      // expand to right (start, stop) + (stop, laterStop)
      val rightBinaryExpansion = for {
        r <- grammar.binarysByLefChild.getOrElse(e.tree.state, Seq())
        laterStop <- stop to n
        rightEdge <- chart.get(EdgeSignature(r.rightChild, (stop, laterStop)))
        newScore = r.score + e.score + rightEdge.score
        newTree = Branch(r.parent, Seq(e.tree, rightEdge.tree))
      } yield Edge(newTree, newScore)
      // expand to left (earlierStart, start) + (start, stop)
      val leftBinaryExpansion = for {
        r <- grammar.binarysByRightmost.getOrElse(e.tree.state, Seq())
        earlierStart <- 0 until start
        leftEdge <- chart.get(EdgeSignature(r.leftChild, (earlierStart, start)))
        newScore = r.score + e.score + leftEdge.score
        newTree = Branch(r.parent, Seq(leftEdge.tree, e.tree))
      } yield Edge(newTree, newScore)

      val allNewEdges = unaryExpansion ++ rightBinaryExpansion ++ leftBinaryExpansion
      allNewEdges.foreach(discoverEdge)
    }
    chart.get(goalSignature).map(e => (e.tree, e.score))
  }
}