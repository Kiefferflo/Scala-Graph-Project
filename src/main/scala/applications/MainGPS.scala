package applications

object MainGPS {
  def main(args: Array[String]): Unit = {
    val start = args(1)
    val end = args(2)
    val (graph, valuationDistance, valuationTemps) = FileReading("???").GPSProcessor
    //On a deux arguments pour FileReading
    // Read qui contient chaque ligne dans une liste de String
    println(graph.shortestPath(valuationDistance)(start,end))
    println(graph.shortestPath(valuationTemps)(start,end))
  }
}
