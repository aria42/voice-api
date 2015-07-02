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
}