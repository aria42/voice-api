package com.pragmaticideal.voiceapi.api

import com.pragmaticideal.voiceapi.api.APIParameterGrammar.APIParameterDerivation
import com.pragmaticideal.voiceapi.cfg.FieldGrammar
import com.pragmaticideal.voiceapi.cfg.parser.AgendaParser
import org.scalatest.{Matchers, FlatSpec}

class APIParameterSpec extends FlatSpec with Matchers {

  "A API parameter grammar" should "parse a simple search request" in {
    val preTriggerPhrases = Map(
      Seq("looking", "for", "articles", "about") -> 1.0,
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