package com.pragmaticideal.voiceapi.cfg

import com.pragmaticideal.voiceapi.cfg.parser.{AgendaParser}
import org.scalatest.{Matchers, FlatSpec}

class CFGParserSpec extends FlatSpec with Matchers {
  "An agenda parser" should "parse a A+B+ simple grammar" in {

    abstract class MyState(terminal: Boolean = false) extends State {
      override def isTerminal = terminal
    }
    object A extends MyState(true)
    object B extends MyState(true)
    object APlus extends MyState()
    object BPlus extends MyState()
    object Root extends MyState()

    val grammar = UnweightedBinaryGrammar(Root,
      Root -> Seq(APlus, BPlus),
      APlus -> Seq(A, APlus),
      APlus -> Seq(A),
      BPlus -> Seq(B, BPlus),
      BPlus -> Seq(B))
    val p = new AgendaParser(grammar)
    val sent= Seq(A, B, B, B)
    val (tree, weight) = p.parseStates(sent).get
    tree.leaves shouldBe sent
  }
}

class SlopPhraseGrammarTest extends FlatSpec with Matchers {

  val phraseSentences = Seq(
    Seq("articles"),
    Seq("search", "articles"),
    Seq("looking","for","articles")
  )

  "A phrase grammar" should "parse simple phrases with no slop" in {
    val weightedPhrases = phraseSentences.map(_ -> 1.0).toMap
    val spg = SlopPhraseGrammar(weightedPhrases, 0, 0.5)
    val parser = new AgendaParser(spg)
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
    val parser = new AgendaParser(spg)

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
    val parser = new AgendaParser(spg)
    val states = spg.states
    for (sent <- sloppySentences) {
      val stateSent: Seq[Map[State, Double]] = for(w <- sent) yield {
        val token = PhraseToken(w)
        if (states.contains(token)) Map[State, Double](token -> 0.0, JunkToken -> 0.0)
        else Map[State, Double](JunkToken -> 0.0)
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
    val parser = new AgendaParser(spg)
    for (sent <- sents) {
      val stateSent = sent.map(PhraseToken)
      val (tree, score) = parser.parseStates(stateSent).get
      assert(tree.leaves.forall(_.isInstanceOf[PhraseToken]))

    }
  }

  "A sloppy phrase grammar" should "have a working lexicon" in {
    val weightedPhrases = phraseSentences.map(_ -> 1.0).toMap
    val spg = SlopPhraseGrammar(weightedPhrases, 2, 0.5)
    val parser = new AgendaParser(spg)
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
    SlopPhraseGrammar(weightedPhrases, 1, 0)
  }
}


class FieldGrammarSpec extends FlatSpec with Matchers {
  "A field grammar" should "respect field weights" in {
    val weigthedWords = Map("w1" -> 1.0, "w2" -> 2.0)
    val fg = FieldGrammar(weigthedWords, 0.1, unkownScore = -0.5)
    val parser = new AgendaParser(fg)
    val expectedPairs = Seq( (Seq("w1"), 1.0), (Seq("w1", "w2"), 3.0 - 0.1),
      (Seq("w1", "junk", "w2"), 3.0 - 0.5 - 0.1 * 2))
    for ((sent, expScore) <- expectedPairs) {
      val (tree, score) = parser.parseSentence(sent).get
      score shouldBe expScore
      assert(tree.leaves.forall(x => x == FieldRoot || x == FieldToken))
    }
  }
}