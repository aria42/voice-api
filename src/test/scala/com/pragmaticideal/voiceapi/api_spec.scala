package com.pragmaticideal.voiceapi.api

import com.pragmaticideal.voiceapi.cfg.parser.AgendaParser
import org.scalatest.{Matchers, FlatSpec}
import org.json4s.jackson.JsonMethods

class APIParameterJsonSpec extends FlatSpec with Matchers {
  "APIParameter spec" should "should extract JSON correctly" in {
    val json =""" {
          "name": "some-param",
          "grammarData": {
            "_type": "unigram",
            "wordScores": {"word3": 1.0, "word4": 2.0},
            "expectedLength": 2
          },
          "preParameter": {
            "weightedPhrases": [{"phrase": ["word1", "word2"], "weight": 1.0}],
            "maxJunk": 1
          }
        }
      """.stripMargin
    implicit val formats = APIParameterSpec.formats
    val jsonObj = JsonMethods.parse(json)
    val apiSpec = jsonObj.extract[APIParameterSpec]
    apiSpec.grammarData shouldBe
      SimpleUnigramFieldAPIParameter(Map("word3" -> 1.0, "word4" -> 2.0), expectedLength = 2)
  }
}


class APIParameterGrammarSpec extends FlatSpec with Matchers {

  "A API parameter grammar" should "parse a simple search request" in {
    val preTriggerPhrases = Map(
      Seq("looking", "for", "articles", "about") -> 1.0,
      Seq("what's", "going", "on", "with") -> 1.0
    )
    val preTriggerSpec = CannedPhraseGrammarSpec(
      (preTriggerPhrases.map { case (p,w) => WeightedPhrase(p,w) }).toSeq,
      maxJunk = 1)
    val fieldGrammar = SimpleUnigramFieldAPIParameter(Map("business" -> 1.0, "obama" -> 1.0), 2)
    val paramGrammar = new APIParameterSpec("search", fieldGrammar, Some(preTriggerSpec))
    val parser = new AgendaParser(paramGrammar.asGrammar)
    val testSentence = Seq("looking", "for", "articles", "about", "obama")
    val sent = Seq("looking", "for", "articles", "about", "obama")
    val (tree, score) = parser.parseSentence(sent).get
    val derivation = APIParameterDerivation.fromTree(tree, sent)
    assert(derivation == Some(APIParameterDerivation("search", Seq("obama"))))
  }
}