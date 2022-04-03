package applications

object MainFreq {
  def main(args: Array[String]): Unit = {
    val nonInter = args(0).toDouble
    val INSEE = args(1)
    val graph = FileReading("./src/main/resources/antennes.csv").AntenneProcessor(nonInter,INSEE)
    println(graph.greedyColoring) //TODO meilleur affichage (dot string ?) c'est compliqué (le graph est trop grand)
  }
}
