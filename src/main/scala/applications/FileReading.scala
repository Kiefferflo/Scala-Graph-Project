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
      (map, str) => map + (str split " (?:: )?" match {
        case x => x.head->x.tail.toSet
      })
    }
    StrictGraphSuccessorsImpl(succ)
  }

  def AntenneProcessor(nonInter : Double, INSEE: Int) : SimpleGraph[String] = AntenneProcessorWithDistance(nonInter,INSEE)._1

  def AntenneProcessorWithDistance(nonInter: Double, INSEE : Int) : (SimpleGraph[String], Map[Edge[String], Double]) = ???
  /*{
    (read foldLeft (Map.empty[String, Set[String]], Map.empty[Edge[String], Double], Map.empty[String, (Int,Int)])) {
      (tuple3,line) => {
        val antenne = line split ";"
        if (antenne(14).toInt == INSEE) (tuple3._3 foldLeft tuple3) {
          (tuple3bis, pair) => (
            tuple3bis._1 match {
              case x if
            },
            tuple3bis._2,
            tuple3bis._3)
        } else tuple3
      }
    }
  }*/

  def calculDistance(long1:Int, lat1:Int) (long2:Int,lat2:Int) : Double = ???
  lazy val GPSProcessor : (StrictGraph[String], Map[Arc[String], Double]) = ???
}
