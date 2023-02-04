package pl.wojciechkarpiel.tableaux
package modal


import modal.*

import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*


class WorldManagerImplTest extends AnyFlatSpec with should.Matchers {

  "World manager" should "work" in {
    val manager = WorldManager.createNew()
    val fromInitA = manager.addReachableFrom(manager.initialWorld)
    val fromInitB = manager.addReachableFrom(manager.initialWorld)
    val fromA = manager.addReachableFrom(fromInitA)
    assert(manager.reachableFrom(manager.initialWorld).toSet == Set(fromInitA, fromInitB))
    assert(manager.isReachableFrom(manager.initialWorld, fromInitA))
    assert(manager.reachableFrom(fromInitA).toSet == Set(fromA))
  }
}
