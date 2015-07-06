package com.pragmaticideal.voiceapi.api

import com.pragmaticideal.voiceapi.cfg.{Parser, AgendaParser}
import org.scalatest.{Matchers, FlatSpec}

class SlopPhraseGrammarTest extends FlatSpec with Matchers {

  val phraseSentences = Seq(
    Seq("articles"),
    Seq("search", "articles"),
    Seq("looking","for","articles")
  )

  "A phrase grammar" should "parse simple phrases with no slop" in {
    val weightedPhrases = phraseSentences.map(_ -> 1.0).toMap
    val spg = SlopPhraseGrammar(weightedPhrases, 0, 0.5)
    val parser: Parser[APIState] = new AgendaParser(spg)
    for (sent <- phraseSentences) {
      val stateSent = sent.map(PhraseToken)
      val (tree, score) = parser.parseStates(stateSent).get
      tree.leaves shouldBe sent.map(PhraseToken)
      score shouldBe 1.0
    }
  }

  val sloppySentences = Seq(
    Seq("looking", "for", "some", "articles"),
    Seq("the", "articles"),
    Seq("articles", "the")
  )

  "A sloppy phrased grammar" should "parse except when a non-grammar state is injected" in {
    val weightedPhrases = phraseSentences.map(_ -> 1.0).toMap
    val spg = SlopPhraseGrammar(weightedPhrases, 1, 0.5)
    val parser: Parser[APIState] = new AgendaParser(spg)

    for (sent <- sloppySentences) {
      val stateSent = sent.map(PhraseToken)
      val thrown = intercept[Exception] {
        parser.parseStates(stateSent)
      }
      assert(thrown != null)
    }

  }
  "A sloppy phrased grammar" should "parse with a single junk word" in {
    val weightedPhrases = phraseSentences.map(_ -> 1.0).toMap
    val spg = SlopPhraseGrammar(weightedPhrases, 1, 0.5)
    val parser: Parser[APIState] = new AgendaParser(spg)
    val states = spg.states
    for (sent <- sloppySentences) {
      val stateSent: Seq[Map[APIState, Double]] = for(w <- sent) yield {
        val token = PhraseToken(w)
        if (states.contains(token)) Map(token -> 0.0, JunkToken -> 0.0)
        else Map(JunkToken.asInstanceOf[APIState] -> 0.0)
      }
      val expectedSent = for (w <- sent) yield {
        val token = PhraseToken(w)
        if (states.contains(token)) token
        else JunkToken
      }
      val (tree, score) = parser.parseTrellis(stateSent).get
      tree.leaves shouldBe expectedSent
    }

  }

  "A sloppy phrased grammar" should "should prefer parse with fewer junk" in {
    val weightedPhrases = phraseSentences.map(_ -> 1.0).toMap
    val spg = SlopPhraseGrammar(weightedPhrases, 2, 0.5)
    val sents = phraseSentences
    val parser: Parser[APIState] = new AgendaParser(spg)
    for (sent <- sents) {
      val stateSent = sent.map(PhraseToken)
      val (tree, score) = parser.parseStates(stateSent).get
      assert(tree.leaves.forall(_.isInstanceOf[PhraseToken]))

    }
  }
}