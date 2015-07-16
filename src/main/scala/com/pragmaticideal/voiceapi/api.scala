/**
 * CFG-representation of API calls and the JSON specs to
 * create grammar
 */
package com.pragmaticideal.voiceapi.api

import com.pragmaticideal.voiceapi.cfg._
import com.pragmaticideal.voiceapi.tree.Tree
import org.json4s

import org.json4s._


// Types below are meant to be read from spec JSON

object ParameterGrammarType extends Enumeration {
  val Unigram = Value
}

case class WeightedPhrase(phrase: Seq[String], weight: Double = 1.0)

case class CannedPhraseGrammarSpec (
  val weightedPhrases: Seq[WeightedPhrase],
  val maxJunk: Int = 0,
  val junkPenalty: Double  = 0.0
)
{
  require(maxJunk >= 0, "can't have negative junk")
  require(junkPenalty >= 0.0, "penalty needs to be non-negative")

  def slopPhraseGrammar: BinaryGrammar = {
    // in case of multiple entries take max weight
    val weightedPhraseMap = weightedPhrases.groupBy(_.phrase).mapValues(entries => entries.map(_.weight).max)
    SlopPhraseGrammar(weightedPhraseMap, maxJunk, junkPenalty)
  }
}

trait GrammarFactory {
  def asGrammar: BinaryGrammar
}

// Root (marker) trait for all the API CFG
trait APIState extends State

case class APIParameterRoot(val name: String) extends APIState
case object APIParameterTarget extends APIState
case object APIParameterPreTarget extends APIState
case object APIParameterTargetWithPreTarget extends APIState

class APIParameterSpec (
  // name of the API parameter
  val name: String,
  // grammar data, type depends on data, but has GrammarFactory trait
  val grammarData: GrammarFactory,
  // canned phrases preceeding the actual parameter value
  val preParameter: Option[CannedPhraseGrammarSpec]
)
{
  def asGrammar = {
    val targetGrammar = grammarData.asGrammar
    val preTargetGrammar: Option[BinaryGrammar] = preParameter.map(_.slopPhraseGrammar)
    val childrenUnaryRules = preTargetGrammar.map(_.unaryRules).getOrElse(Seq()) ++
      targetGrammar.unaryRules
    val childrenBinaryRules = preTargetGrammar.map(_.binaryRules).getOrElse(Seq()) ++
      targetGrammar.binaryRules
    val root = APIParameterRoot(name)
    // The grammar is a lot simpler if there is no pre target grammar
    val topLevelRules: Seq[Rule] = preTargetGrammar match {
      case Some(pre) =>
        Seq[Rule](
          UnaryRule(root, APIParameterTargetWithPreTarget),
          UnaryRule(APIParameterPreTarget, pre.root),
          UnaryRule(APIParameterTarget, targetGrammar.root),
          UnaryRule(APIParameterTargetWithPreTarget, APIParameterTarget),
          BinaryRule(APIParameterTargetWithPreTarget, APIParameterPreTarget, APIParameterTarget)
        )
      case None =>
        Seq[Rule](
          UnaryRule(root, APIParameterTarget),
          UnaryRule(APIParameterTarget, targetGrammar.root)
        )
    }
    val topLevelUnarys = topLevelRules.filter(_.isInstanceOf[UnaryRule]).map(_.asInstanceOf[UnaryRule])
    val topLevelBinarys = topLevelRules.filter(_.isInstanceOf[BinaryRule]).map(_.asInstanceOf[BinaryRule])
    val lexicon = new Lexicon {
      override def wordTrellis(words: Seq[String]) = {
        val preTrellis = preTargetGrammar.flatMap(_.lexicon).map(_.wordTrellis(words))
        val targetTrellis = targetGrammar.lexicon.get.wordTrellis(words)
        for (idx <- 0 until words.length) yield
        preTrellis.map(_(idx)).getOrElse(Map()) ++ targetTrellis(idx)
      }
    }
    BinaryGrammar(root,
      topLevelUnarys ++ childrenUnaryRules,
      topLevelBinarys ++ childrenBinaryRules,
      Some(lexicon))
  }
}

object APIParameterSpec {


  class GrammarDataSerializer extends CustomSerializer[GrammarFactory]( fmt => (
      {
        // Deserialize JSON. Take out the "_type" key and dispatch based on that
        // value and extract an instance from rest of JSON object with approriate class
        case JObject(JField("_type", JString(grammarType)) :: rest) => {
          grammarType match {
            case "unigram" => JObject(rest).extract[SimpleUnigramFieldAPIParameter]
            case _ => throw new MappingException(s"Unknown type $grammarType")
          }

        }
        case other => throw new MappingException(s"Unable to map $other")
      },
      {
        // To Serialize to JSON, serialize the object itself and then
        // provide the "_type" type-hint key based on the class. Must
        // be symmetrical with deserialization aboe
        case obj =>
          import org.json4s.JsonDSL._
          val rest = Extraction.decompose(obj).asInstanceOf[JObject]
          rest ~ ("_type", obj match {
            case _ : SimpleUnigramFieldAPIParameter => "unigram"
            case _ => throw new MappingException(s"Don't have a short type-hint for ${obj.getClass.getSimpleName}")
          })
      }
    ))

  implicit val formats = new DefaultFormats {
    override val customSerializers = List(new GrammarDataSerializer)
  }
}

case class SimpleUnigramFieldAPIParameter(
    val wordScores: Map[String, Double],
    val expectedLength: Int,
    val unknownWordProb: Double = 0.01) extends GrammarFactory {
  override def asGrammar = {
    require(wordScores.values.forall(_ >= 0.0), "All word scores need to be positive")
    val wordScoreTotal = wordScores.values.sum
    require(wordScoreTotal > 0.0, "need to have at least one positive weight word")
    // Make each word weight a log probability with mass reserved for junk
    val wordLogProbs = wordScores.mapValues(w => Math.log((1.0 - unknownWordProb) * w/wordScoreTotal))
    val geometricStopProb = 1.0/expectedLength.toDouble
    // In log-space since weighted CFG are additive,
    // technically also need a special stop_token
    // TODO(aria42) Add special stop token to end a phrase, to make valid distribution
    val lengthPenalty = -Math.log(1.0-geometricStopProb)
    FieldGrammar(wordLogProbs, lengthPenalty, Math.log(unknownWordProb))
  }
}

case class APIParameterDerivation(name: String, field: Seq[String])

object APIParameterDerivation {
  def fromTree(tree: Tree, sent: Seq[String]): Option[APIParameterDerivation] = {
    require(tree.state.isInstanceOf[APIParameterRoot], "Not a API parameter tree")
    require(tree.leaves.length == sent.length, "Tree needs to agree in length with sentence")
    val name: Option[String] = tree.state match {
      case APIParameterRoot(n) => Some(n)
      case _ => None
    }
    name.flatMap { n =>
      val fieldRoot = tree.findFirstBFS(s => s == FieldRoot)
      fieldRoot map { root =>
        val (start, stop) = root.span
        val phrase = sent.drop(start).take(stop-start)
        APIParameterDerivation(n, phrase)
      }
    }
  }
}