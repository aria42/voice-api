package com.pragmaticideal.voiceapi.api

import com.pragmaticideal.voiceapi.api.APIParameterGrammar.APIParameterDerivation
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
        if (states.contains(token)) Map[APIState, Double](token -> 0.0, JunkToken -> 0.0)
        else Map[APIState, Double](JunkToken -> 0.0)
      }
      val expectedSent = for (w <- sent) yield {
        val token = PhraseToken(w)
        if (states.contains(token)) token
        else JunkToken
      }
      val (tree, score) = parser.parseLattice(stateSent).get
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

  "A sloppy phrase grammar" should "have a working lexicon" in {
    val weightedPhrases = phraseSentences.map(_ -> 1.0).toMap
    val spg = SlopPhraseGrammar(weightedPhrases, 2, 0.5)
    val parser: Parser[APIState] = new AgendaParser(spg)
    for (sent <- sloppySentences ++ phraseSentences) {
      assert(parser.parseSentence(sent).isDefined)
    }
  }

  "A sloppy phrase grammar" should "not allow negative max slop" in {
    val weightedPhrases = phraseSentences.map(_ -> 1.0).toMap
    intercept[IllegalArgumentException] {
      SlopPhraseGrammar(weightedPhrases, -1, 0.5)
    }
  }
  "A sloppy phrase grammar" should "not allow for negative weights" in {
    val weightedPhrases = phraseSentences.map(_ -> 1.0).toMap
    intercept[IllegalArgumentException] {
      SlopPhraseGrammar(weightedPhrases, 1, -0.5)
    }
  }
}

class FieldGrammarSpec extends FlatSpec with Matchers {
  "A field grammar" should "respect field weights" in {
    val weigthedWords = Map("w1" -> 1.0, "w2" -> 2.0)
    val fg = FieldGrammar(weigthedWords, 0.1, unkownScore = -0.5)
    val parser: Parser[APIState] = new AgendaParser(fg)
    val expectedPairs = Seq( (Seq("w1"), 1.0), (Seq("w1", "w2"), 3.0 - 0.1),
      (Seq("w1", "junk", "w2"), 3.0 - 0.5 - 0.1 * 2))
    for ((sent, expScore) <- expectedPairs) {
      val (tree, score) = parser.parseSentence(sent).get
      score shouldBe expScore
      assert(tree.leaves.forall(x => x == FieldRoot || x == FieldToken))
    }
  }
}

class APIParameterSpec extends FlatSpec with Matchers {

  "A API parameter grammar" should "parse a simple search request" in {
    val preTriggerPhrases = Map(
      Seq("looking", "for", "articles", "about") -> -1.0,
      Seq("what's", "going", "on", "with") -> 1.0
    )
    val fieldGrammar = FieldGrammar(Map("business" -> 1.0, "obama" -> 1.0))
    val paramGrammar = APIParameterGrammar("search", preTriggerPhrases, fieldGrammar, Map())
    val parser = new AgendaParser(paramGrammar)
    val testSentence = Seq("looking", "for", "articles", "about", "obama")
    val sent = Seq("looking", "for", "articles", "about", "obama")
    val (tree, score) = parser.parseSentence(sent).get
    val derivation = APIParameterDerivation.fromTree(tree, sent)
    assert(derivation == Some(APIParameterDerivation("search", Seq("obama"))))
  }
}