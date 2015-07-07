package com.pragmaticideal.voiceapi.cfg

import scala.collection.mutable

trait State {
  def isTerminal = false
}

trait Rule[S <: State] {
  def parent: S
  def children: Seq[S]
  def score: Double
}

case class BinaryRule[S <: State](
  override val parent: S,
  val leftChild: S,
  val rightChild: S,
  override val score: Double = 0.0) extends Rule[S] {
  override def children = Seq(leftChild, rightChild)
}

case class UnaryRule[S <: State](
      override val parent: S,
      val child: S,
      override val score: Double = 0.0) extends Rule[S] {
  override def children = Seq(child)
}

case class NAryRule[S <: State](
      override val parent: S,
      override val children: Seq[S],
      override val score: Double = 0.0) extends Rule[S]

trait Lexicon[S <: State] {
  /**
   * Each of the state keys should be terminal grammar states
   */
  def wordTrellis(words: Seq[String]): Seq[Map[S, Double]]
}

trait Grammar[S <: State] {
  def root: S
  def rules: Seq[Rule[S]]

  def states: Set[S] = (for {
    r <- rules
    s <- r.parent +: r.children
  } yield s).toSet

  def terminalStates = states.filter(_.isTerminal)

  def nonTerminalStates = states.filterNot(_.isTerminal)

  /**
   * A grammar may have a lexicon associated with it
   */
  def lexicon: Option[Lexicon[S]]
}

case class BinaryGrammar[S <: State](
      override val root: S,
      val unaryRules: Seq[UnaryRule[S]],
      val binaryRules: Seq[BinaryRule[S]],
      override val lexicon: Option[Lexicon[S]] = None) extends Grammar[S] {
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
  def apply[S <: State](root: S, xs: (S, Seq[S])*): BinaryGrammar[S] = {
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
abstract class Tree[S <: State](val state: S, val children: Seq[Tree[S]]) {

  def leaves: Seq[S]

  def findFirstBFS(pred: S => Boolean): Option[Tree[S]] = {
    if (pred(this.state)) {
      return Some(this)
    }
    children.map(_.findFirstBFS(pred)).find(_.isDefined) match {
      case Some(x) => x
      case None => None
    }
  }

  def span: (Int, Int)
}

case class Leaf[S <: State](override val state: S, val tokenIndex: Int) extends Tree[S](state, Seq()) {
  override def leaves = Seq(state)

  override def toString = state.toString

  override def span = (tokenIndex, tokenIndex + 1)
}

case class Branch[S <: State](override val state: S, override val children: Seq[Tree[S]])
  extends Tree[S](state, children)
{
  override def leaves = children.flatMap(_.leaves)

  def treeString(indentLevel: Int): String = {
    val build = new StringBuilder()
    build.append("  " * indentLevel)
    build.append("(" + state.toString)
    val childStr = children.map(_.toString).mkString(" ")
    if (childStr.length < 80) {
      build.append(" ")
      build.append(childStr)
    } else {
      build.append("\n")
      build.append(children.map {
        case b @ Branch(_, _)  => b.treeString(indentLevel+1)
        case l @ Leaf(_, _) => l.toString
      }.mkString(" "))
    }
    build.append(")")
    build.toString
  }

  override  def toString = treeString(0)

  override def span = (children.head.span._1, children.last.span._2)
}

// Parser
trait Parser[S <: State]  {

  def parseStates(states: Seq[S]): Option[(Tree[S], Double)] = parseTrellis(states.map(s => Map(s -> 0.0)))

  def parseTrellis(weightedStates: Seq[Map[S, Double]]): Option[(Tree[S], Double)]

  def parseSentence(sentence: Seq[String]): Option[(Tree[S], Double)]
}

class AgendaParser[S <: State](val grammar: BinaryGrammar[S])
    extends Parser[S]
{
  case class Edge(val tree: Tree[S], val score: Double) {
    def signature = EdgeSignature(tree.state, span)
    def span = tree.span
    def length = span._2 - span._1
  }

  case class EdgeSignature(val state: S, val span: (Int, Int))

  override def parseSentence(sentence: Seq[String]): Option[(Tree[S], Double)] = {
    require(grammar.lexicon.isDefined, "Need a lexicon to parse raw sentence")
    parseTrellis(grammar.lexicon.get.wordTrellis(sentence))
  }

  override def parseTrellis(sentence: Seq[Map[S, Double]]): Option[(Tree[S], Double)] = {
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