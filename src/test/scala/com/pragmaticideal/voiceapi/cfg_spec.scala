package com.pragmaticideal.voiceapi.cfg

import org.scalatest.{Matchers, FlatSpec}

class CFGParserSpec extends FlatSpec with Matchers {
  "An agenda parser" should "parse a A+B+ simple grammar" in {
    object State extends Enumeration {
      type State = Value
      val A, APlus, B, BPlus, Root = Value
    }
    import State._
    val grammar = Grammar(Root,
      Root -> Seq(APlus, BPlus),
      APlus -> Seq(A, APlus),
      APlus -> Seq(A),
      BPlus -> Seq(B, BPlus),
      BPlus -> Seq(B))
    val p: Parser[State] = new AgendaParser(grammar)
    val sent= Seq(A, B, B, B)
    val tree = p.parse(sent)
    tree.isDefined shouldBe true
    tree.get.leaves shouldBe sent
  }
}