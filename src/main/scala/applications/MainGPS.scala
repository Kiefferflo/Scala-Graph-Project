package applications

object MainGPS {
  def main(args: Array[String]): Unit = {
    val start = args(1)
    val end = args(2)
    val (graph, valuation) = FileReading("???").GPSProcessor //TODO resources du GPS
    println(graph.shortestPath(valuation)(start,end))
  }
}
