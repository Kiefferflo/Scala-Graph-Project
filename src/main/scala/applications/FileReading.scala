package applications

import directed._
import undirected._


import scala.io.Source
import scala.math.{Pi, asin, cos, pow, sin, sqrt}

case class FileReading(FileName: String) {
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
      AntenneProcessorOneLigne(nonInter, INSEE)(_,_)
    }

    (SimpleGraphNeighborsImpl(tuple3._1), tuple3._2)
  }

  def AntenneProcessorOneLigne(nonInter : Double, INSEE: Int)(tuple3: (Map[String, Set[String]], Map[Edge[String], Double],
    Map[String, (Int, Int, Int, String, Int, Int, Int, String)]), line : String) :
  (Map[String, Set[String]], Map[Edge[String], Double], Map[String, (Int, Int, Int, String, Int, Int, Int, String)]) = {
    val antenne = line split ";"
    if (antenne(14).toInt == INSEE) (tuple3._3 foldLeft (tuple3._1 + (antenne(0) -> Set.empty[String]),tuple3._2,tuple3._3)) {
      AntenneProcessorAntenneByAntenne(nonInter, antenne)(_, _)
    } else tuple3
  }

  def AntenneProcessorAntenneByAntenne(nonInter: Double, antenne: Array[String])
                                      (tuple3: (Map[String, Set[String]], Map[Edge[String], Double], Map[String, (Int, Int, Int, String, Int, Int, Int, String)]),
                                       pair: (String, (Int, Int, Int, String, Int, Int, Int, String))) :
  (Map[String, Set[String]], Map[Edge[String], Double], Map[String, (Int, Int, Int, String, Int, Int, Int, String)]) = {
    val longAndLat = (antenne(3).toInt,antenne(4).toInt,antenne(5).toInt,antenne(6),antenne(7).toInt,antenne(8).toInt,antenne(9).toInt,antenne(10))
    if (calculDistance(pair._2)(longAndLat) < nonInter)
      (
        tuple3._1 + (antenne(0) -> (tuple3._1(antenne(0)) + pair._1)),
        tuple3._2 + (Edge(antenne(0),pair._1) -> calculDistance(pair._2)(longAndLat)),
        tuple3._3 + (antenne(0) -> longAndLat)
      )
    else tuple3
  }


  def calculDistance(longAndLat1:(Int,Int,Int,String,Int,Int,Int,String))(longAndLat2:(Int,Int,Int,String,Int,Int,Int,String)) : Double = {
    val long1=longAndLat1._1+(longAndLat1._2/60)+(longAndLat1._3/3600)
    val lat1=longAndLat1._5+(longAndLat1._6/60)+(longAndLat1._7/3600)
    val long2=longAndLat2._1+(longAndLat2._2/60)+(longAndLat2._3/3600)
    val lat2=longAndLat2._5+(longAndLat2._6/60)+(longAndLat2._7/3600)
    val long1bis = if(longAndLat1._4=="S") -1*long1 else long1
    val long2bis = if(longAndLat2._4=="S") -1*long2 else long2
    val lat1bis = if(longAndLat1._8=="W") -1*lat1 else lat1
    val lat2bis = if(longAndLat2._8=="W") -1*lat2 else  lat2

    val dLat=(lat2bis - lat1bis)*180/Pi
    val dLon=(long2bis - long1bis)*180/Pi
    val a = pow(sin(dLat/2),2) + pow(sin(dLon/2),2) * cos(lat1*180/Pi) * cos(lat2*180/Pi)
    val c = 2 * asin(sqrt(a))
    val R = 6372.8
    R * c
  }

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


