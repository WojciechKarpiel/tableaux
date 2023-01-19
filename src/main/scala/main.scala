package pl.wojciechkarpiel.tableaux

import lang.Term
import lang.Term.Unifiable

import pl.wojciechkarpiel.tableaux.util.Gensym


@main
def main(): Unit = {
  val a1 = TestA(2)
  val a2 = TestA(2)
  val b1 = TestB(2)
  val b2 = TestB(2)

  val a: Term = new Unifiable()
  val g = new Gensym()
  val g2 = Gensym()
  println(s"Hello world! ${a1 == a2} ${b1 == b2}, $a, $g, $g2")
}

class TestA(val i:Int)
case class TestB( i:Int)