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

  def calculDistance(long1_dg:Int, long1_mn: Int, long1_sc:Int, long1_ns: String, lat1_dg:Int, lat1_mn: Int, lat1_sc:Int, lat1_ew: String)
                    (long2_dg:Int, long2_mn: Int, long2_sc:Int, long2_ns: String, lat2_dg:Int, lat2_mn: Int, lat2_sc:Int, lat2_ew: String) : Double = ???
  lazy val GPSProcessor : (StrictGraph[String], Map[Arc[String], Double]) = ???
}
