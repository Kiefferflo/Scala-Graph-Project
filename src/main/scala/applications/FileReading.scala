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

  def AntenneProcessor(nonInter : Double, INSEE: String) : SimpleGraph[String] = AntenneProcessorWithDistance(nonInter,INSEE)._1

  def AntenneProcessorWithDistance(nonInter: Double, INSEE : String) : (SimpleGraph[String], Map[Edge[String], Double]) =
  {
    val tuple3 = (read drop 1 foldLeft (Map.empty[String, Set[String]], Map.empty[Edge[String], Double], Map.empty[String, (Int,Int,Int,String,Int,Int,Int,String)])) {
      AntenneProcessorOneLigne(nonInter, INSEE)(_,_)
    }

    (SimpleGraphNeighborsImpl(tuple3._1), tuple3._2)
  }

  def AntenneProcessorOneLigne(nonInter : Double, INSEE: String)(tuple3: (Map[String, Set[String]], Map[Edge[String], Double],
    Map[String, (Int, Int, Int, String, Int, Int, Int, String)]), line : String) :
  (Map[String, Set[String]], Map[Edge[String], Double], Map[String, (Int, Int, Int, String, Int, Int, Int, String)]) = {
    val antenne = line split ";"
    if (antenne.length == 15 && antenne(14) == INSEE) {
      val longAndLat = (antenne(3).toInt, antenne(4).toInt, antenne(5).toInt, antenne(6), antenne(7).toInt, antenne(8).toInt, antenne(9).toInt, antenne(10))
      (tuple3._3 foldLeft (tuple3._1 + (antenne(0) + ";" + antenne(1) -> Set.empty[String]), tuple3._2, tuple3._3 + (antenne(0) + ";" + antenne(1) -> longAndLat))) {
        AntenneProcessorAntenneByAntenne(nonInter, antenne)(longAndLat)(_, _)
      }
    } else tuple3
  }

  def AntenneProcessorAntenneByAntenne(nonInter: Double, antenne: Array[String])
                                      (longAndLat : (Int,Int,Int,String,Int,Int,Int,String))
                                      (tuple3: (Map[String, Set[String]], Map[Edge[String], Double], Map[String, (Int, Int, Int, String, Int, Int, Int, String)]),
                                       pair: (String, (Int, Int, Int, String, Int, Int, Int, String)))
  : (Map[String, Set[String]], Map[Edge[String], Double], Map[String, (Int, Int, Int, String, Int, Int, Int, String)]) = {
    val distance = calculDistance(pair._2,longAndLat)
    if (distance < nonInter)
      (
        (tuple3._1 + ((antenne(0) + ";" + antenne(1)) -> (tuple3._1(antenne(0) + ";" + antenne(1)) + pair._1)))
          + (pair._1 -> (tuple3._1(pair._1) + (antenne(0) + ";" + antenne(1)))),
        tuple3._2 + (Edge(antenne(0) + ";" + antenne(1),pair._1) -> distance),
        tuple3._3
      )
    else tuple3
  }


  def calculDistance(longAndLat1:(Int,Int,Int,String,Int,Int,Int,String),longAndLat2:(Int,Int,Int,String,Int,Int,Int,String)) : Double = {
    val long1=longAndLat1._1+(longAndLat1._2/60)+(longAndLat1._3/3600)
    val lat1=longAndLat1._5+(longAndLat1._6/60)+(longAndLat1._7/3600)
    val long2=longAndLat2._1+(longAndLat2._2/60)+(longAndLat2._3/3600)
    val lat2=longAndLat2._5+(longAndLat2._6/60)+(longAndLat2._7/3600)
    val long1bis = if(longAndLat1._4=="S") -1*long1 else long1
    val long2bis = if(longAndLat2._4=="S") -1*long2 else long2
    val lat1bis = if(longAndLat1._8=="W") -1*lat1 else lat1
    val lat2bis = if(longAndLat2._8=="W") -1*lat2 else  lat2

    val dLat=(lat2bis - lat1bis)*180*Pi
    val dLon=(long2bis - long1bis)*180*Pi
    val a = pow(sin(dLat/2),2) + pow(sin(dLon/2),2) * cos(lat1*180*Pi) * cos(lat2*180*Pi)
    val c = 2 * asin(sqrt(a))
    val R = 6372.8
    R * c
  }

  lazy val GPSGraph : StrictGraphSuccessorsImpl[String] = {
    val succ = (read foldLeft Map.empty[String,Set[String]]) {
      (map, str) => map + (str split " (?:; )?" match {
        // v; v| d | t; v| d | t; v| d | t; v| d | t; v| d | t
        case x => x.head->{ for (e<-x.tail) yield (e split " (?:| )?")(0) }.toSet })
    }
    StrictGraphSuccessorsImpl(succ)
  }

  /*
  lazy val GPSGraph2 : Map[Arc[String], (Double,Double)] = {
    val succ = (read foldLeft Map.empty[Arc[String], (Double,Double)]) {
      (map, str) => map + str split " (?:; )?" match {
        // v; v| d | t; v| d | t; v| d | t; v| d | t; v| d | t
        case x => {
          for (e<-x.tail) yield (Arc(x.head,(e split " (?:| )?")(0)),
            ((e split " (?:| )?")(1).toDouble,(e split " (?:| )?")(2).toDouble) ) }.toSet
      }
    }
    succ
  }

   */

  lazy val GPSGraphFirstDecomposition : Set[(Arc[String], (Double,Double))] = {
    val succ = (read foldLeft Set.empty[(Arc[String], (Double,Double))]) {
      (map, str) => map + str split " (?:; )?" match {
        // v; v| d | t; v| d | t; v| d | t; v| d | t; v| d | t
        case x => {
          for (e<-x.tail) yield (Arc(x.head,(e split " (?:| )?")(0)),
            ((e split " (?:| )?")(1).toDouble,(e split " (?:| )?")(2).toDouble) ) }.toSet
      }
    }
    succ
  } // Retourne un set de tuple qui contient un arc associe a sa distance et son temps

  lazy val GPSGraphSecond : ( StrictGraph[String],Map[Arc[String], Double],Map[Arc[String], Double] ) = {
    val succ = (GPSGraphFirstDecomposition foldLeft (StrictGraphDefaultImpl[String], Map.empty[Arc[String],Double],Map.empty[Arc[String],Double] ) ) {
      (truple, nextElement) => {
        val current_arc = nextElement._1
        val current_distance = nextElement._2._1
        val current_temps= nextElement._2._2
        ( (truple._1.apply().+|(nextElement._1) ), //Problem TODO
        (truple._2 + (current_arc -> current_distance)),
        (truple._3 + (current_arc -> current_temps)))
      }
    }
    succ
  }

  lazy val GPSMapOfDistance : Map[Arc[String], Double] = ???
  /*
  lazy val GPSMapOfDistance : Map[Arc[String], Double] = {
    val myMap = (read foldLeft Map.empty[Arc[String], Double]) {
      (map, str) =>
        map + (str split " (?:: )?" match {
          // v: v| d | t: v| d | t: v| d | t: v| d | t: v| d | t
          case x => {
            for (e <- x.tail) yield (
              (GPSGraph.getArcFromVertice(x.head, e split " (?:| )?"(0))) match {
              Some (arc_i) => (arc_i -> e split " (?:| )?" (1) )
              }
              )
          }.toSet
        })
    }
    myMap
  }
  */

  lazy val GPSMapOfTime : Map[Arc[String], Double] = ???
  lazy val GPSProcessor : (StrictGraph[String],Map[Arc[String], Double],Map[Arc[String], Double]) = {
    //cle: sommet: valeur
    ( GPSGraph, GPSMapOfDistance,GPSMapOfTime)
  }


}


