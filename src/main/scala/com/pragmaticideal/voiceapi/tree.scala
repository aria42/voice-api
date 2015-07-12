package com.pragmaticideal.voiceapi.tree

import com.pragmaticideal.voiceapi.cfg.{State}

// Tree Abstraction
abstract class Tree(val state: State, val children: Seq[Tree]) {

  def leaves: Seq[State]

  def findFirstBFS(pred: State => Boolean): Option[Tree] = {
    if (pred(this.state)) {
      return Some(this)
    }
    children.map(_.findFirstBFS(pred)).find(_.isDefined) match {
      case Some(x) => x
      case None => None
    }
  }

  def span: (Int, Int)
}

case class Leaf(override val state: State, val tokenIndex: Int) extends Tree(state, Seq()) {
  override def leaves = Seq(state)

  override def toString = state.toString

  override def span = (tokenIndex, tokenIndex + 1)
}

case class Branch(override val state: State, override val children: Seq[Tree])
  extends Tree(state, children)
{
  override def leaves = children.flatMap(_.leaves)

  def treeString(indentLevel: Int): String = {
    val build = new StringBuilder()
    build.append("  " * indentLevel)
    build.append("(" + state.toString)
    val childStr = children.map(_.toString).mkString(" ")
    if (childStr.length < 80) {
      build.append(" ")
      build.append(childStr)
    } else {
      build.append("\n")
      build.append(children.map {
        case b @ Branch(_, _)  => b.treeString(indentLevel+1)
        case l @ Leaf(_, _) => l.toString
      }.mkString(" "))
    }
    build.append(")")
    build.toString
  }

  override  def toString = treeString(0)

  override def span = (children.head.span._1, children.last.span._2)
}
