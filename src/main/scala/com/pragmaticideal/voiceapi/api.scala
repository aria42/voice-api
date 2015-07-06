/**
 * CFG-representation of API calls
 */
package com.pragmaticideal.voiceapi.api

import com.pragmaticideal.voiceapi.cfg._
import scala.collection.mutable

// Root trait for all the API CFG
trait APIState extends State

// A token of the user utterance explicit in grammar
case class PhraseToken(val token: String) extends APIState {
  override def isTerminal = true
}

// A token not matching what's in the grammar
object JunkToken extends APIState

// State representing a complete canned phrase
case class SlopPhrase(val phrase: Seq[String], val numJunk: Int) extends APIState

// A slop phrase of any amount of junk
object SlopPhraseRoot extends APIState

object SlopPhraseGrammar {

  type S = APIState
  type R = Rule[S]

  private def slopRules(phrase: Seq[String], weight: Double, maxSlop: Int, junkPenalty: Double): Seq[R] = {
    val rules = mutable.ArrayBuffer[R]()
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
    } yield BinaryRule(parent, PhraseToken(word), rightChild).asInstanceOf[R])
    // Junk token can be the first in the phrase (above rules only allow it
    for (numJunk <- 0 until maxSlop) {
      rules += BinaryRule(SlopPhrase(phrase, numJunk+1), JunkToken, SlopPhrase(phrase, numJunk))
    }
    rules
  }

  def apply(phrases: Map[Seq[String], Double], maxSlop: Int, junkPenalty: Double): BinaryGrammar[APIState] = {
    val root = SlopPhraseRoot
    val allRules = phrases.flatMap {
      case (phrase, weight) => slopRules(phrase, weight, maxSlop, junkPenalty)
    }
    val unaryRules = allRules
      .filter(_.isInstanceOf[UnaryRule[S]])
      .map(_.asInstanceOf[UnaryRule[S]])
      .toSet
    val binaryRules = allRules
      .filter(_.isInstanceOf[BinaryRule[S]])
      .map(_.asInstanceOf[BinaryRule[S]])
      .toSet
    val allTerms: Set[String] = phrases.keys.flatMap(ks => ks).toSet
    val lexicon = new Lexicon[APIState] {
      override def wordTrellis(words: Seq[String]) = for (word <- words) yield {
        if (allTerms(word)) Map(PhraseToken(word) -> 0.0, JunkToken -> 0.0)
        else Map(JunkToken.asInstanceOf[APIState] -> 0.0)
      }
    }
    BinaryGrammar(root, unaryRules.toSeq, binaryRules.toSeq, Some(lexicon))
  }
}

object FieldToken extends APIState {
  override def isTerminal = true
}

object FieldRoot extends APIState

object FieldGrammar {
  /**
   * Field grammar is like a slop grammar, but it doesn't care about how many junk tokens there are. It's
   * less about a canned phrase and intended for free text field arguments that might have some prefferred
   * unigrams.
   */
  def apply(weightedWords: Map[String, Double], lengthPenalty: Double, unkownScore: Double = 0.0): BinaryGrammar[APIState] = {
    val unaryRules = Seq(UnaryRule(FieldRoot, FieldToken))
    val binaryRules = Seq(BinaryRule(FieldRoot, FieldRoot, FieldToken, -lengthPenalty))
    val lexicon = new Lexicon[APIState] {
      override def wordTrellis(words: Seq[String]) =
        for (w <- words) yield Map(FieldToken.asInstanceOf[APIState] -> weightedWords.getOrElse(w, unkownScore))
    }
    BinaryGrammar(FieldRoot, unaryRules, binaryRules, Some(lexicon))
  }
}

object APIParameterGrammar {

  type WeightedPhrases = Map[Seq[String], Double]

  def apply(preTriggerPhrases: WeightedPhrases,
            fieldGrammar: BinaryGrammar[APIState],
            postTriggerPhrases: WeightedPhrases): (BinaryGrammar[APIState], Lexicon[APIState]) = ???
}