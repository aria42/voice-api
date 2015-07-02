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
    if (phrase.length == 1) {
      rules += UnaryRule(SlopPhrase(phrase, 0), PhraseToken(phrase(0)))
    }
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
    BinaryGrammar(root, unaryRules.toSeq, binaryRules.toSeq)
  }
}
