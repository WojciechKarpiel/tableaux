package pl.wojciechkarpiel.tableaux
package util

/**
 * All instances pretend to be equal so that to not mess up holder equality
 */
class MetadataHolder(val isFunction: Boolean) {
  override def hashCode(): Int = 0

  override def equals(obj: Any): Boolean = true
}
