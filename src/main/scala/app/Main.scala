package pl.wojciechkarpiel.tableaux
package app

import tree.Tree

import scala.annotation.tailrec
import scala.io.StdIn.readLine

object Main {

  def main(args: Array[String]): Unit = {

    var maxSearchDepth: Int = 4
    var printDebug: Boolean = false
    var innerDebug: Boolean = false

    println("Spróbuj wpisać jakie co, na przykład:")
    println("Paradoks barowy: jeśli jeden pije, to wszyscy piją")
    println("exists x. forall y. (Pije(x) => Pije(y))")
    println("Albo udowodnij istnienie liczby 3")
    println("N(0) ∧ ∀i.((N(i) ⇒ N(s(i)))) ⇒ N(s(s(s(0))))")
    println("Albo coś modalnego:")
    println("∀x.□F(x) ⇒ □∀x.F(x)")
    println("◇(p ⇒ q) ⇒ □p ⇒ ◇q")
    println(s"Głębokość poszukiwań to $maxSearchDepth")
    println(s"Żeby ją zmienić podaj liczbę zamiast formuły logicznej")

    @tailrec
    def loop(): Unit = {
      print("> ")
      val input = readLine()
      if (input != null && input.nonEmpty) {
        if input == "debug" then {
          printDebug = !printDebug
          println(s"Debug mode ${if printDebug then "enabled" else "disabled"}")
        } else if input == "odpluskw" then {
          innerDebug = !innerDebug
        } else input.toIntOption match
          case Some(newBound) =>
            println(s"Nowa głębokość poszukiwań: $newBound (było: $maxSearchDepth)")
            maxSearchDepth = newBound
          case None =>
            val startTime = System.nanoTime()
            val tree = new Tree(input, innerDebug)
            val proven = tree.solve(maxSearchDepth).isDefined
            val endTime = System.nanoTime()
            val response = if proven then s"To prawda, że ${tree.formula}"
            else s"Nie udało mi się udowodnić, że ${tree.formula} (głębokość szukania: $maxSearchDepth)"
            println(response)
            println(s"Operacja zajęła ${formatTime(endTime - startTime)}")
            if (printDebug) tree.printTree()
        loop()
      }
    }

    loop()
  }


  private def formatTime(nanos: Long) = {
    val base = 1000L

    def log1000(x: Long): Int = if x == 0 then -1 else 1 + log1000(x / base)

    def naivePow(base: Long, exp: Long): Long = if exp == 0 then 1 else base * naivePow(base, exp - 1)

    val log = log1000(nanos)
    val unit = units(log)
    val threshold = naivePow(base, log)
    s"${nanos / threshold},${nanos % threshold}$unit"
  }

  private val units = Seq("s", "ms", "μs", "ns").reverse
}
