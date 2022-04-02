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

  def AntenneProcessorWithDistance(nonInter: Double, INSEE : Int) : (SimpleGraph[String], Map[Edge[String], Double]) =
  {
    val tuple3 = (read foldLeft (Map.empty[String, Set[String]], Map.empty[Edge[String], Double], Map.empty[String, (Int,Int,Int,String,Int,Int,Int,String)])) {
      (tuple3tmp,line) => {
        val antenne = line split ";"
        if (antenne(14).toInt == INSEE) (tuple3tmp._3 foldLeft (tuple3tmp._1 + (antenne(0) -> Set.empty[String]),tuple3tmp._2,tuple3tmp._3)) {
          (tuple3tmpbis, pair) =>
            val longAndLat = (antenne(3).toInt,antenne(4).toInt,antenne(5).toInt,antenne(6),antenne(7).toInt,antenne(8).toInt,antenne(9).toInt,antenne(10))
            if (calculDistance(pair._2)(longAndLat) < nonInter)
              (tuple3tmpbis._1 + (antenne(0) -> (tuple3tmpbis._1(antenne(0)) + pair._1)),
              tuple3tmpbis._2 + (Edge(antenne(0),pair._1) -> calculDistance(pair._2)(longAndLat)),
              tuple3tmpbis._3 + (antenne(0) -> longAndLat)
              )
            else tuple3tmpbis
        } else tuple3tmp
      }
    }

    (SimpleGraphNeighborsImpl(tuple3._1), tuple3._2)
  }

  def calculDistance(longAndLat1:(Int,Int,Int,String,Int,Int,Int,String))
                    (longAndLat2:(Int,Int,Int,String,Int,Int,Int,String)) : Double = ???

  lazy val GPSGraph : StrictGraphSuccessorsImpl[String] = {
    val succ = (read foldLeft Map.empty[String,Set[String]]) {
      (map, str) => map + (str split " (?:: )?" match {
        // v: v| d | t: v| d | t: v| d | t: v| d | t: v| d | t
        case x => x.head->{ for (e<-x.tail) yield (e split " (?:| )?")(0) }.toSet })
    }
    StrictGraphSuccessorsImpl(succ)
  }

  lazy val GPSMapOfDistance : Map[Arc[String], Double] = ???
  lazy val GPSMapOfTime : Map[Arc[String], Double] = ???
  lazy val GPSProcessor : (StrictGraph[String],Map[Arc[String], Double],Map[Arc[String], Double]) = {
    //cle: sommet: valeur
    ( GPSGraph, GPSMapOfDistance,GPSMapOfTime)
  }


}


