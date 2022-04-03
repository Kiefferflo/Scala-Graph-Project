package applications

object MainFreq {
  def main(args: Array[String]): Unit = {
    val nonInter = args(0).toDouble
    val INSEE = args(1)
    val graph = FileReading("./src/main/resources/testA.csv").AntenneProcessor(nonInter,INSEE)
    println(graph.toDOTString)
    println(fromColorToDot(graph.greedyColoring))
    println(graph.greedyColoring) //TODO meilleur affichage (dot string ?) c'est compliqué (le graph est trop grand)
  }
  val color : Map[Int, String] = Map(
    (0,"red"),
    (1,"aliceblue"),
    (2,"antiquewhite"),
    (3,"blue1"),
    (4,"brown"),
    (5,"chartreuse1"),
    (6,"coral"),
    (7,"darkseagreen1"),
    (8,"aquamarine"),
    (9,"green")
  )
  def fromColorToDot(colors: Map[String, Int]): String =
    (colors foldLeft "") {(str, pair) => str + pair._1 + " [style=filled, fillcolor=" + color(pair._2) + "]\n"}
}
