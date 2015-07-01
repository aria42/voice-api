package com.pragmaticideal.voiceapi.cfg

import scala.collection.mutable

trait Rule[S] {
  def parent: S
  def children: Seq[S]
  def score: Double
}

case class BinaryRule[S](override val parent: S,
                         val leftChild: S,
                         val rightChild: S,
                         override val score: Double = 0.0) extends Rule[S] {
  override def children = Seq(leftChild, rightChild)
}

case class UnaryRule[S](override val parent: S,
                        val child: S,
                        override val score: Double = 0.0) extends Rule[S] {
  override def children = Seq(child)
}

case class NAryRule[S](override val parent: S,
                       override val children: Seq[S],
                       override val score: Double = 0.0) extends Rule[S]

trait Grammar[S] {
  def root: S
  def rules: Seq[Rule[S]]
}
case class BinaryGrammar[S](override val root: S,
                            val unaryRules: Seq[UnaryRule[S]],
                            val binaryRules: Seq[BinaryRule[S]]) extends Grammar[S] {
  type UR = UnaryRule[S]
  type BR = BinaryRule[S]
  // Index rules by children states
  val unarysByChild: Map[S, Seq[UR]] = unaryRules.groupBy(_.child)
  val binarysByLefChild: Map[S, Seq[BR]] = binaryRules.groupBy(_.leftChild)
  val binarysByRightmost: Map[S , Seq[BR]] = binaryRules.groupBy(_.rightChild)
  override def rules = unaryRules ++ binaryRules
}

// convenience factory to be able to use X -> Y syntax for unweighted grammars
object UnweightedBinaryGrammar {
  def apply[S](root: S, xs: (S, Seq[S])*): BinaryGrammar[S] = {
    val unaryRules = xs.filter(_._2.length == 1).map {
      case (parent, Seq(child)) => UnaryRule[S](parent, child)
    }
    val binaryRules = xs.filter(_._2.length == 2).map {
      case (parent, Seq(left, right)) => BinaryRule(parent, left, right)
    }
    BinaryGrammar(root, unaryRules, binaryRules)
  }
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

class AgendaParser[S](val grammar: BinaryGrammar[S]) extends Parser[S] {

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
    val terminalEdges = for {
      (s, idx) <- sentence.zipWithIndex
      leaf = Leaf(s)
      r <- grammar.unarysByChild.getOrElse(s, Seq())
    } yield Edge(leaf, (idx, idx+1), r.score)
    terminalEdges.foreach(discoverEdge)
    val goalSignature = EdgeSignature(grammar.root, (0, n))
    // finished when we have a goal edge or out of edges
    def isFinished = agenda.headOption.map(_.signature == goalSignature).getOrElse(true)
    while (!isFinished) {
      val e = agenda.dequeue
      // unary expansion bottom-up
      val unaryExpansion = for {
        r <- grammar.unarysByChild.getOrElse(e.tree.state, Seq())
        newTree = Branch(r.parent, Seq(e.tree))
      } yield Edge(newTree, e.span, e.score + r.score)
      val (start, stop) = e.span
      // expand to right (start, stop) + (stop, laterStop)
      val rightBinaryExpansion = for {
        r <- grammar.binarysByLefChild.getOrElse(e.tree.state, Seq())
        laterStop <- stop to n
        rightEdge <- chart.get(EdgeSignature(r.rightChild, (stop, laterStop)))
        newScore = r.score + e.score + rightEdge.score
        newTree = Branch(r.parent, Seq(e.tree, rightEdge.tree))
      } yield Edge(newTree, (start, laterStop), newScore)
      // expand to left (earlierStart, start) + (start, stop)
      val leftBinaryExpansion = for {
        r <- grammar.binarysByRightmost.getOrElse(e.tree.state, Seq())
        earlierStart <- 0 until start
        leftEdge <- chart.get(EdgeSignature(r.leftChild, (earlierStart, start)))
        newScore = r.score + e.score + leftEdge.score
        newTree = Branch(r.parent, Seq(leftEdge.tree, e.tree))
      } yield Edge(newTree, (earlierStart, stop), newScore)

      val allNewEdges = unaryExpansion ++ rightBinaryExpansion ++ leftBinaryExpansion
      allNewEdges.foreach(discoverEdge)
    }
    chart.get(goalSignature).map(_.tree)
  }
}