package cz.cvut.fit.prl.scala.implicits

import better.files._
import java.io.{File => JFile}
import java.nio.file.Paths

import scala.meta.internal.semanticdb.Locator

object MergeSemanticdbs extends App {

  val pathFile = args(0)
  val paths = File(pathFile).lines.map(File(_).path).toList

  Locator(paths) { (path, sdb) =>
    // get the project id
    //
  }


}
