package com.pragmaticideal.voiceapi.cfg

import scala.collection.mutable

case class Rule[S](val parent: S,  val children: Seq[S], val score: Double = 0.0) {
  // rules are unary or binary only
  require(children.length == 1 || children.length == 2)
  def isUnary = children.length == 1
  def isBinary = children.length == 2
  def leftmostChild = children.head
  def rightmostChild = children.last
}

class Grammar[S](val root: S, val rules: Seq[Rule[S]]) {
  require(rules.filter(_.parent == root).nonEmpty, s"Grammar needs some rule with root $root as parent")
  // Index rules by children states
  val unarysByChild: Map[S, Seq[Rule[S]]] = rules.filter(_.isUnary).groupBy(_.leftmostChild)
  val binarysByLeftmost: Map[S, Seq[Rule[S]]] = rules.filter(_.isBinary).groupBy(_.leftmostChild)
  val binarysByRightmost: Map[S , Seq[Rule[S]]] = rules.filter(_.isBinary).groupBy(_.rightmostChild)
}

// convenience factory to be able to use X -> Y syntax for unweighted grammars
object Grammar {
  def apply[S](root: S, xs: (S, Seq[S])*) = new Grammar(root, xs.map(t => Rule(t._1, t._2)))
}

// Tree Abstraction
abstract class Tree[S](val state: S, val children: Seq[Tree[S]]) {
  def leaves: Seq[S]
}

case class Leaf[S](override val state: S) extends Tree[S](state, Seq()) {
  override def leaves = Seq(state)
}

case class Branch[S](override val state: S, override val children: Seq[Tree[S]])
  extends Tree[S](state, children)
{
  override def leaves = children.flatMap(_.leaves)
}

// Parser
trait Parser[S]  {
  def parse(sentence: Seq[S]): Option[Tree[S]]
}

class AgendaParser[S](val grammar: Grammar[S]) extends Parser[S] {

  case class Edge(val tree: Tree[S], val span: (Int, Int), val score: Double) {
    def signature = EdgeSignature(tree.state, span)
  }

  case class EdgeSignature(val state: S, val span: (Int, Int))

  override def parse(sentence: Seq[S]): Option[Tree[S]] = {
    val n = sentence.length
    // Agenda is a PQ on edges priortized on higher scores
    val agenda = mutable.PriorityQueue[Edge]()(Ordering.by(_.score))
    // Chart stores best (first-encountered) derivation of EdgeSignature
    val chart = mutable.Map[EdgeSignature, Edge]()
    // If you discover a new edge, add it to chart/agenda, explore later
    def discoverEdge(edge: Edge) {
      if (!chart.contains(edge.signature)) {
        chart.put(edge.signature, edge)
        agenda += edge
      }
    }
    // base case: discover terminal edges
    for {
      (s, idx) <- sentence.zipWithIndex
      leaf = Leaf(s)
      r <- grammar.unarysByChild(s)
    } discoverEdge(Edge(leaf, (idx, idx+1), r.score))
    val goalSignature = EdgeSignature(grammar.root, (0, n))
    // finished when we have a goal edge or out of edges
    def isFinished = agenda.headOption.map(_.signature == goalSignature).getOrElse(true)
    while (!isFinished) {
      val e = agenda.dequeue
      // unary expansion bottom-up
      for (r <- grammar.unarysByChild.getOrElse(e.tree.state, Seq())) {
        val newTree = Branch(r.parent, Seq(e.tree))
        discoverEdge(Edge(newTree, e.span, e.score + r.score))
      }
      val (start, stop) = e.span
      // expand to right (start, stop) + (stop, laterStop)
      for {
        r <- grammar.binarysByLeftmost.getOrElse(e.tree.state, Seq())
        laterStop <- stop to n
        rightEdge <- chart.get(EdgeSignature(r.rightmostChild, (stop, laterStop)))
        newScore = r.score + e.score + rightEdge.score
        newTree = Branch(r.parent, Seq(e.tree, rightEdge.tree))
        newEdge = Edge(newTree, (start, laterStop), newScore)
      } discoverEdge(newEdge)
      // expand to left (earlierStart, start) + (start, stop)
      for {
        r <- grammar.binarysByRightmost.getOrElse(e.tree.state, Seq())
        earlierStart <- 0 until start
        leftEdge <- chart.get(EdgeSignature(r.leftmostChild, (earlierStart, start)))
        newScore = r.score + e.score + leftEdge.score
        newTree = Branch(r.parent, Seq(leftEdge.tree, e.tree))
        newEdge = Edge(newTree, (earlierStart, stop), newScore)
      } discoverEdge(newEdge)
    }
    chart.get(goalSignature).map(_.tree)
  }
}