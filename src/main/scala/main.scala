package pl.wojciechkarpiel.tableaux

@main
def main(): Unit = {
  val a1 = TestA(2)
  val a2 = TestA(2)
  val b1 = TestB(2)
  val b2 = TestB(2)
  println(s"Hello world! ${a1==a2} ${b1==b2}")
}

class TestA(val i:Int)
case class TestB( i:Int)