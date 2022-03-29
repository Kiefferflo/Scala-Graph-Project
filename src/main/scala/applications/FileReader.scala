package applications

import undirected.SimpleGraph

import scala.io.Source

class FileReader(val FileName: String) {
  def Read(): List[String] = {
    (Source fromFile FileName).getLines.toList
  }
/*
  def antenneProcessor[T[V] <: SimpleGraph[V]]() : T = {

  }

 */
}
