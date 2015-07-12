/**
 * CFG-representation of API calls
 */
package com.pragmaticideal.voiceapi.api

import com.pragmaticideal.voiceapi.cfg._
import com.pragmaticideal.voiceapi.tree.Tree

// Root (marker) trait for all the API CFG
trait APIState extends State

case class APIParameterRoot(val name: String) extends APIState
case object APIParameterTarget extends APIState
case object APIParameterPreTarget extends APIState
case object APIParameterPostTrigger extends APIState
// to make this a binary grammar easily
case object APIParameterTargetWithPreTarget extends APIState

object APIParameterGrammar {

  type WeightedPhrases = Map[Seq[String], Double]

  def apply(name: String,
            preTarget: WeightedPhrases,
            targetGrammar: BinaryGrammar,
            postTarget: WeightedPhrases,
            maxSlop: Int = 2,
            junkPenalty: Double = 0.5): BinaryGrammar =
  {
    val preTargetGrammar = SlopPhraseGrammar(preTarget, maxSlop, junkPenalty)
    val postTargetGrammar = SlopPhraseGrammar(postTarget, maxSlop, junkPenalty)
    val childrenUnaryRules = preTargetGrammar.unaryRules ++
      postTargetGrammar.unaryRules ++
      targetGrammar.unaryRules
    val childrenBinaryRules = preTargetGrammar.binaryRules ++
      postTargetGrammar.binaryRules ++
      targetGrammar.binaryRules

    val root = APIParameterRoot(name)
    val topLevelUnarys = Seq[UnaryRule](
      UnaryRule(APIParameterPreTarget, preTargetGrammar.root),
      UnaryRule(APIParameterPostTrigger, postTargetGrammar.root),
      UnaryRule(APIParameterTarget, targetGrammar.root),
      UnaryRule(APIParameterTargetWithPreTarget, APIParameterTarget),
      UnaryRule(root, APIParameterTargetWithPreTarget)
    )

    val topLevelBinarys = Seq[BinaryRule](
      BinaryRule(APIParameterTargetWithPreTarget, APIParameterPreTarget, APIParameterTarget),
      BinaryRule(root, APIParameterTargetWithPreTarget, APIParameterPostTrigger)
    )

    val lexicon = new Lexicon {
      override def wordTrellis(words: Seq[String]) = {
        val preTrellis = preTargetGrammar.lexicon.get.wordTrellis(words)
        val postTrellis = postTargetGrammar.lexicon.get.wordTrellis(words)
        val targetTrellis = targetGrammar.lexicon.get.wordTrellis(words)
        for (idx <- 0 until words.length) yield
          preTrellis(idx) ++ targetTrellis(idx) ++ postTrellis(idx)
      }
    }

    BinaryGrammar(root,
      topLevelUnarys ++ childrenUnaryRules,
      topLevelBinarys ++ childrenBinaryRules,
      Some(lexicon))
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
}