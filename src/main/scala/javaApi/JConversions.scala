package pl.wojciechkarpiel.tableaux
package javaApi

import java.util
import scala.jdk.CollectionConverters.*

private object JConversions {

  def asJava[A](list: Seq[A]): util.List[A] = list.asJava

  def asScala[A](list: util.List[A]): Seq[A] = list.asScala.toSeq
}
