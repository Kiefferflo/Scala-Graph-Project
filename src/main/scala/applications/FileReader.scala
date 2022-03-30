package applications

import directed._
import undirected.SimpleGraph
import scala.io.Source

class FileReader(val FileName: String) {
  lazy val read: List[String] = {
    (Source fromFile FileName).getLines.toList
  }

  def MakeProcessor() : StrictGraphSuccessorsImpl[String] = {
    val succ =(read foldLeft Map.empty[String,Set[String]]) {
      (map, str) => map + ((str split " (?:: )?").toSeq match {
        case q :+ h => h->q.toSet
      })
    }
    StrictGraphSuccessorsImpl(succ)
  }


}
