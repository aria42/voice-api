package com.pragmaticideal.voiceapi.cfg

import scala.collection.mutable

trait State {
  def isTerminal = false
}

trait Rule {
  def parent: State
  def children: Seq[State]
  def score: Double
}

case class BinaryRule(
  override val parent: State,
  val leftChild: State,
  val rightChild: State,
  override val score: Double = 0.0) extends Rule {
  override def children = Seq(leftChild, rightChild)
}

case class UnaryRule(
      override val parent: State,
      val child: State,
      override val score: Double = 0.0) extends Rule {
  override def children = Seq(child)
}

case class NAryRule(
      override val parent: State,
      override val children: Seq[State],
      override val score: Double = 0.0) extends Rule

trait Lexicon {
  /**
   * Each of the state keys should be terminal grammar states
   */
  def wordTrellis(words: Seq[String]): Seq[Map[State, Double]]
}

trait Grammar {
  def root: State
  def rules: Seq[Rule]

  def states: Set[State] = (for {
    r <- rules
    s <- r.parent +: r.children
  } yield s).toSet

  def terminalStates = states.filter(_.isTerminal)

  def nonTerminalStates = states.filterNot(_.isTerminal)

  /**
   * A grammar may have a lexicon associated with it
   */
  def lexicon: Option[Lexicon]
}

case class BinaryGrammar(
      override val root: State,
      val unaryRules: Seq[UnaryRule],
      val binaryRules: Seq[BinaryRule],
      override val lexicon: Option[Lexicon] = None) extends Grammar {
  type UR = UnaryRule
  type BR = BinaryRule
  // Index rules by children states
  val unarysByChild: Map[State, Seq[UR]] = unaryRules.groupBy(_.child)
  val binarysByLefChild: Map[State, Seq[BR]] = binaryRules.groupBy(_.leftChild)
  val binarysByRightmost: Map[State, Seq[BR]] = binaryRules.groupBy(_.rightChild)
  override def rules = unaryRules ++ binaryRules
}

// A token of the user utterance explicit in grammar
case class PhraseToken(val token: String) extends State {
  override def isTerminal = true
}

// A token not matching what's in the grammar
case object JunkToken extends State

// State representing a complete canned phrase
case class SlopPhrase(val phrase: Seq[String], val numJunk: Int) extends State

// A slop phrase of any amount of junk
case object SlopPhraseRoot extends State

object SlopPhraseGrammar {

  private def slopRules(phrase: Seq[String], weight: Double, maxSlop: Int, junkPenalty: Double): Seq[Rule] = {
    val rules = mutable.ArrayBuffer[Rule]()
    // Top-level rule for each amount of junk, this has the weight of the phrase

    for (numJunk <- 0 to maxSlop) {
      // Root -> SlopPhrase(phrase, numJunk)
      rules += UnaryRule(SlopPhraseRoot, SlopPhrase(phrase, numJunk), weight - numJunk * junkPenalty)
    }
    // The grammar is left binarizing so need a special unary for rightmost token
    rules += UnaryRule(SlopPhrase(Seq(phrase.last), 0), PhraseToken(phrase.last))
    // Two types of binary rules
    // Take an existing partial phrase and add a right junk token
    rules ++= (for {
      (word, idx) <- phrase.zipWithIndex
      phraseSoFar = phrase.drop(idx)
      numJunk <- 0 until maxSlop
      parent = SlopPhrase(phraseSoFar, numJunk+1)
      leftChild = SlopPhrase(phraseSoFar, numJunk)
    } yield BinaryRule(parent, leftChild, JunkToken))
    // Extend a partial phrase with a right phrase token
    rules ++= (for {
      (word, idx) <- phrase.zipWithIndex
      if idx + 1 < phrase.length
      before = phrase.drop(idx)
      after = phrase.drop(idx+1)
      numJunk <- 0 to maxSlop
      parent = SlopPhrase(before, numJunk)
      rightChild = SlopPhrase(after, numJunk)
    } yield BinaryRule(parent, PhraseToken(word), rightChild))
    // Junk token can be the first in the phrase (above rules only allow it
    for (numJunk <- 0 until maxSlop) {
      rules += BinaryRule(SlopPhrase(phrase, numJunk+1), JunkToken, SlopPhrase(phrase, numJunk))
    }
    rules
  }

  def apply(phrases: Map[Seq[String], Double], maxSlop: Int, junkPenalty: Double): BinaryGrammar = {
    require(maxSlop >= 0, s"Max slop must be positive.")
    require(junkPenalty >= 0, s"Penalties must be positive")
    val root = SlopPhraseRoot
    val allRules = phrases.flatMap {
      case (phrase, weight) => slopRules(phrase, weight, maxSlop, junkPenalty)
    }
    val unaryRules = allRules
      .filter(_.isInstanceOf[UnaryRule])
      .map(_.asInstanceOf[UnaryRule])
      .toSet
    val binaryRules = allRules
      .filter(_.isInstanceOf[BinaryRule])
      .map(_.asInstanceOf[BinaryRule])
      .toSet
    val allTerms: Set[String] = phrases.keys.flatMap(ks => ks).toSet
    val lexicon = new Lexicon {
      override def wordTrellis(words: Seq[String]) = for (word <- words) yield {
        if (allTerms(word)) Map[State,Double](PhraseToken(word) -> 0.0, JunkToken -> 0.0)
        else Map[State,Double](JunkToken -> 0.0)
      }
    }
    BinaryGrammar(root, unaryRules.toSeq, binaryRules.toSeq, Some(lexicon))
  }
}

// convenience factory to be able to use X -> Y syntax for unweighted grammars
object UnweightedBinaryGrammar {
  def apply[S <: State](root: S, xs: (S, Seq[S])*): BinaryGrammar = {
    val unaryRules = xs.filter(_._2.length == 1).map {
      case (parent, Seq(child)) => UnaryRule(parent, child)
    }
    val binaryRules = xs.filter(_._2.length == 2).map {
      case (parent, Seq(left, right)) => BinaryRule(parent, left, right)
    }
    BinaryGrammar(root, unaryRules, binaryRules)
  }
}

case object FieldToken extends State {
  override def isTerminal = true
}
case object FieldPhrase extends State
case object FieldRoot extends State

object FieldGrammar {
  /**
   * Field grammar is like a slop grammar, but it doesn't care about how many junk tokens there are. It's
   * less about a canned phrase and intended for free text field arguments that might have some prefferred
   * unigrams.
   */
  def apply(weightedWords: Map[String, Double],
            lengthPenalty: Double = 0.1,
            unkownScore: Double = 0.0): BinaryGrammar =
  {
    val unaryRules = Seq(UnaryRule(FieldRoot, FieldPhrase), UnaryRule(FieldPhrase, FieldToken))
    val binaryRules = Seq(BinaryRule(FieldPhrase, FieldPhrase, FieldToken, -lengthPenalty))
    val lexicon = new Lexicon {
      override def wordTrellis(words: Seq[String]) =
        for (w <- words) yield Map(FieldToken.asInstanceOf[State] -> weightedWords.getOrElse(w, unkownScore))
    }
    BinaryGrammar(FieldRoot, unaryRules, binaryRules, Some(lexicon))
  }
}
