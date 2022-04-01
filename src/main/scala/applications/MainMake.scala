package applications
import java.nio.file.Paths

object MainMake {
  def main(args: Array[String]): Unit = {
    val graph = FileReading("./src/main/resources/Makefile").MakeProcessor
    println(graph.topologicalOrder) //TODO Faire un meilleur affichage
  }
}
