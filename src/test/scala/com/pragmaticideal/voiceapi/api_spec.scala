package com.pragmaticideal.voiceapi.api

import com.pragmaticideal.voiceapi.cfg.{UnweightedBinaryGrammar, Parser, Grammar, AgendaParser}
import org.scalatest.{Matchers, FlatSpec}

class TextFieldGramar extends FlatSpec with Matchers {

  val phraseSentences = Seq(
    Seq("articles"),
    Seq("search", "articles"),
    Seq("looking","for","articles")
  )

  "A phrase grammar" should "parse simple phrases with no slop" in {
    val weightedPhrases = phraseSentences.map(_ -> 1.0).toMap
    val tfg = SlopPhraseGrammar(weightedPhrases, 0, 0.5)
    val parser: Parser[APIState] = new AgendaParser(tfg)
    for (sent <- phraseSentences) {
      val stateSent = sent.map(PhraseToken)
      val (tree, score) = parser.parseSentence(stateSent).get
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
    val tfg = SlopPhraseGrammar(weightedPhrases, 1, 0.5)
    val parser: Parser[APIState] = new AgendaParser(tfg)

    for (sent <- sloppySentences) {
      val stateSent = sent.map(PhraseToken)
      val thrown = intercept[Exception] {
        parser.parseSentence(stateSent)
      }
      assert(thrown != null)
    }

  }
  "A sloppy phrased grammar" should "parse with a single junk word" in {
    val weightedPhrases = phraseSentences.map(_ -> 1.0).toMap
    val tfg = SlopPhraseGrammar(weightedPhrases, 1, 0.5)
    val parser: Parser[APIState] = new AgendaParser(tfg)
    val states = tfg.states
    for (sent <- sloppySentences) {
      val stateSent = for(w <- sent) yield {
        if (states.contains(PhraseToken(w))) Map(PhraseToken(w) -> 0.0, JunkToken -> 0.0)
        else Map(JunkToken.asInstanceOf[APIState] -> 0.0)
      }
      val expectedSent = for (w <- sent) yield {
        if (states.contains(PhraseToken(w))) PhraseToken(w)
        else JunkToken
      }
      val (tree, score) = parser.parse(stateSent).get
      tree.leaves shouldBe expectedSent
    }

  }
}