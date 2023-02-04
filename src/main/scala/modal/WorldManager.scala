package pl.wojciechkarpiel.tableaux
package modal

import modal.WorldManager.UnknownWorld

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

trait WorldManager {
  def reachableFrom(world: World): Seq[World]

  def isReachableFrom(from: World, to: World): Boolean = reachableFrom(from).contains(to)

  /**
   * @return new world
   */
  def addReachableFrom(from: World): World

  def initialWorld: World
}


object WorldManager {
  final class UnknownWorld(world: World) extends RuntimeException(s"Unknown world $world")

  def createNew(): WorldManager = new WorldManagerImpl()
}

final class WorldManagerImpl extends WorldManager:

  private val reachabilityMap: mutable.Map[World, mutable.Buffer[World]] = new mutable.HashMap()

  override val initialWorld: World =
    val initial = new World()
    addWorld(initial)
    initial

  def addReachableFrom(from: World): World =
    val newWorld = new World()
    val reachableWorlds = reachableWorldsInternal(from)
    addWorld(newWorld)
    reachableWorlds += newWorld
    newWorld

  def reachableFrom(world: World): Seq[World] = reachableWorldsInternal(world).toSeq

  private def reachableWorldsInternal(from: World): mutable.Buffer[World] =
    reachabilityMap.getOrElse(from, throw new UnknownWorld(from))

  private def addWorld(newWorld: World): Unit = reachabilityMap.put(newWorld, new ArrayBuffer[World]())
