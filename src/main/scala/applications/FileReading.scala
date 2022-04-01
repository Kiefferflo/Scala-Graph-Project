package applications

import directed._
import undirected._

import java.nio.file.Paths
import scala.io.Source

case class FileReading(val FileName: String) {
  lazy val read: List[String] = {
    (Source fromFile FileName).getLines.toList filterNot { x => x.isEmpty && x.isBlank}
  }
  lazy val MakeProcessor : StrictGraphSuccessorsImpl[String] = {
    val succ = (read foldLeft Map.empty[String,Set[String]]) {
      (map, str) => map + ((str split " (?:: )?") match {
        case x => x.head->x.tail.toSet
      })
    }
    StrictGraphSuccessorsImpl(succ)
  }

  def AntenneProcessor(nonInter : Double, INSEE: Int) : SimpleGraph[String] = AntenneProcessorWithDistance(nonInter,INSEE)._1

  def AntenneProcessorWithDistance(nonInter: Double, INSEE : Int) : (SimpleGraph[String], Map[Edge[String], Double]) = ???

  lazy val GPSProcessor : (StrictGraph[String],Map[Arc[String], Double]) = ???
}
