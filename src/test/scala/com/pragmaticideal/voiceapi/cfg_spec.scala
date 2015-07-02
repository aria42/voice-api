package com.pragmaticideal.voiceapi.cfg

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
    val (tree, weight) = p.parseSentence(sent).get
    tree.leaves shouldBe sent
  }
}