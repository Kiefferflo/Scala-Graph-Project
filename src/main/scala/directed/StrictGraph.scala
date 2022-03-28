package directed

import scala.annotation.tailrec
//Merci Florent, beni des cieux
/** Trait for a directed ''and strict'' graph, i.e. without loop nor parallel arcs */
trait StrictGraph[V] {
    /* QUERY METHODS */

    /** The set of all vertices of the graph */
    val vertices : Set[V]

    /** The set of all     arcs of the graph */
    val arcs : Set[Arc[V]]

    /** The set of all vertices with arcs incoming from input vertex
      * @param v vertex
      * @return [[None]] if `v` is not an actual vertex, the set of all successors of `v` otherwise
      */
    def successorsOf(v : V) : Option[Set[V]]


    /** The number of incoming arcs to input vertex
      * @param v vertex
      * @return [[None]] if `v` is not an actual vertex, the inner degree of `v` otherwise
      */
    def inDegreeOf(v : V) : Option[Int] =
        if (vertices.contains(v)) Some( // le point entré appartient à la liste des points
            numberOfArcsByDestination(v) //Check from numberOfArcsByDestination

        ) else None


    lazy val numberOfArcsByOrigin : Map[V, Int] =
        arcs groupBy(_._1) map {case(o->ar) => (o->ar.size)}
    lazy val numberOfArcsByDestination : Map[V, Int] =
        arcs groupBy(_._2) map {case(d->ar) => (d->ar.size)}

    /**
    lazy val myPath : Map[V, V] =
        (arcs foldLeft Map.empty[V,V]) {
            (m,ar) => m + (ar._1->ar._2)
            }
    */

    /** The arc with the vertex given as parameters
     * @param  x1 Begining of the arc
     * @param x2 End of the arc
     * @return [[None]] if arc doesn't exist, the arc if it does
     */
    def getArcFromVertice(x1:V,x2:V): Option[Arc[V]] = arcs.find(x => (x._1==x1) && (x._2==x2))




    /** The number of outcoming arcs to input vertex
      * @param v vertex
      * @return [[None]] if `v` is not an actual vertex, the outer degree of `v` otherwise
      */
    def outDegreeOf(v : V) : Option[Int] =
        if (vertices.contains(v)) Some( // le point entré appartient à la liste des points
            numberOfArcsByOrigin(v) //Check from numberOfArcsByDestination
        ) else None

    /** The number of adjacent vertices to input vertex
      * @param v vertex
      * @return [[None]] if `v` is not an actual vertex, the degree of `v` otherwise
      */
    def degreeOf(v : V) : Option[Int] = if (vertices.contains(v)) Some( // le point entré appartient à la liste des points
        numberOfArcsByOrigin(v) + numberOfArcsByDestination(v) //TODO WARNING May cause problem if arcs goes both ways
    ) else None

    /* VERTEX OPERATIONS */

    /** Add vertex to graph
      * @param v new vertex
      * @return the graph with new vertex `v`
      *         if `v` is an actual vertex of graph, return input graph
      */
    def + (v : V) : StrictGraph[V]

    /** Remove vertex from graph
      * @param v new vertex
      * @return the graph without vertex `v`
      *         if `v` is not an actual vertex of graph, return input graph
      */
    def - (v : V) : StrictGraph[V]

    /* ARC OPERATIONS */

    /** Add arc to graph (also add arc ends as new vertices if necessary)
      * @param a new arc
      * @return the graph with new arc `e`
      *         if `e` is an actual arc of graph, return input graph
      */
    def +| (a : Arc[V]) : StrictGraph[V]

    /** Remove arc from graph (does NOT remove ends)
      * @param a new arc
      * @return the graph without arc `e`
      *         if `e` is not an actual arc of graph, return input graph
      */
    def -| (a : Arc[V]) : StrictGraph[V]

    /** Remove all arcs from graph but keep same vertices
      * @return graph with same vertices without any arc
      */
    def withoutArc : StrictGraph[V]

    /** Add all possible arc with same vertices
      * @return graph with same vertices and all possible arcs
      */
    def withAllArcs : StrictGraph[V]

    /* SEARCH METHODS */

    /** A topological order of the vertex set (if exists)
     * To know if it exist, we class it, then compare the number of elements */
    lazy val topologicalOrder : Option[Seq[V]] =
        if (topologicalBeginning.size==vertices.size) topologicalBeginning else None


    //Créer une fonction recursive: Cherche tous ceux avec out=0
    // Et les mets en memoire V1.
    // Puis, on cherche ceux dont out ne connecte qu'à ceux en V1,
    // On les stock dans V2. Puis V1=V1+V2, et V2 = Void.
    // On recommance jusqu'à ce que V2 = Void

    /**
     * Return the topological ordering of the vertices through recursivity
     * @return
     */
    def topologicalBeginning: Option[Seq[V]] = {

        val myVal = (for (v<-vertices if (outDegreeOf(v) match {
              case Some(x) if x== 0=> true
              case Some(_) => false
              case None => false
        }))
        yield v).toSeq
        if (myVal==None) None else Some(topologicalRecursive(myVal))
    }

    /**
     *
     * @param alreadyOrdered Vertex already sorted
     * @return Vertex sorted
     *  To work, we create NouvValTrie, for vertex to add to our sorted pool
     *  If NouvValTrie is void, then we reached the end
     */
    def topologicalRecursive(alreadyOrdered: Seq[V]): Seq[V] = {
        val nouvValTrie = (for (v<-vertices if canAddToTopological(v,alreadyOrdered)) yield v).toSeq
        if (nouvValTrie==None) alreadyOrdered else topologicalRecursive(alreadyOrdered ++ nouvValTrie)
    }

    /**
     * Return if a vertex can be added to the pool of sorted vertex
     * @param v Vertex to test
     * @param x Vertex already sorted
     * @return False if v is in option, else, allDestinationsAlreadyCovered
     */
    def canAddToTopological(v: V, x: Seq[V]): Boolean = {
        if (x contains v) false else allDestinationsAlreadyCovered(v, x)
    }

    /**
     * Return true if all destinations of v are in vertex
     * @param v Vertex tested
     * @param option Pool of sorted vertex
     * @return Return true if all destinations of v are in vertex
     */
    def allDestinationsAlreadyCovered(v: V, option: Seq[V]): Boolean = {
        val inelegantWay = for (ar<- arcs if ar._1==v) yield option.contains(ar._2)
        //On enregistre tous les arcs partant de v, et si option contient la destination
        !inelegantWay.contains(false)
        //Retourne false si inelegantWay contient false, true sinon
    }

    /**
    * Return True if all elements from the first Seq are in the second
     */

    /* VALUATED GRAPH METHODS */

    /**
     * Dijktra's Map: Map[V,(V,Double)]
     * x1:V -> (x2:V , d:Double)
     * x1   : The key, this is the vertex concerned
     * x2   : What is the previous vertex to use to reach this vertex in the minimum amount distance
     * d    : Total distance to reach this point from 'start' through x2
     */

    /** Computes a shortest path between two vertices
      * @param valuation valuation of graph
      * @param start origin      of path
      * @param end   destination of path
      * @return [[None]] if there is no path from `start` to `end`, the shortest path and its valuation otherwise
      */
    def shortestPath(valuation : Map[Arc[V], Double])(start : V, end : V) : Option[(Seq[V], Double)] = {
        val thankYouKieffer = initMyCalculation(valuation)(start)
        if (thankYouKieffer contains end) {
            Some((readFromEnd(thankYouKieffer, start, end, Seq.empty[V]),thankYouKieffer(end)._2))
        } else None
    }

    /** Computes a shortest path between two vertices
     * Use recursivity
     * @param thankYouKieffer valuation of graph
     * @param start origin      of path
     * @param end   destination of path
     * @return [[None]] if there is no path from `start` to `end`, the shortest path and its valuation otherwise
     */
    @tailrec final def readFromEnd(thankYouKieffer: Map[V,(V,Double)], start:V, current:V, accumul:Seq[V]):Seq[V] = {
        if (current!=start) readFromEnd(thankYouKieffer,start,
                                        thankYouKieffer(current)._1,
                                        (accumul :+ current))
        else accumul :+ current
    }


    /**
     * Initialisation to Dijktra's algorithm
     * @param valuation a map giving the distance of an arc
     * @param start Where to start Dijktra
     * @return
     */
    def initMyCalculation(valuation : Map[Arc[V], Double])(start:V): Map[V,(V,Double)] = {
        MyCalculation(valuation)(
            (vertices foldLeft Map.empty[V,(V,Double)]) {
                (m,v) => v match {
                    case x if x == start  =>  m + (v->(v,0.0))
                    case _              => m + (v->(v,99999.0))
                }
            },start,0.0)
    }

    /**
     * Dijktra's algorithm
     * @param valuation a map giving the distance of an arc
     * @param accumulationToHere Dijktra's map (see before) up to this point
     * @param currentlyAt Where to apply Dijktra
     * @param currentLength The length from start to here.
     *                      We could do without by reading from Dijktra's map, but it was hard enough to keep track of everything as is
     * @return Dijktra's map, possibly improved
     */
    final def MyCalculation(valuation : Map[Arc[V], Double])(accumulationToHere : Map[V,(V,Double)], currentlyAt:V, currentLength:Double) : Map[V,(V,Double)] = {
        val MyMap = (accumulationToHere foldLeft Map.empty[V, (V, Double)]) {
            (m, v) =>
                v._2 match { //Si la longueur enregistree est superieurs a la longueur actuelle
                    case (_, value) if value > valuator(valuation)(currentlyAt, v._1) =>
                        m + (v._1 -> (currentlyAt, currentLength + valuator(valuation)(currentlyAt, v._1)))
                    case _ => m + (v)
                }
        }
        // We want to do a 'Deep Search', with "MyMap" being actualised along the way
        /*
        (MyMap,successorsOf(currentlyAt)) match {
            case (y,Some(x)) if (y!=accumulationToHere) =>
                (x foldLeft MyMap) { (m, suc) => (MyCalculation(valuation)(m, suc, currentLength + valuator(valuation)(currentlyAt, suc)) ) }
            case (_,_)                                  => MyMap // Doesn't work, to investigate
        }
         */

        if (MyMap != accumulationToHere)
            successorsOf(currentlyAt) match {
                case None => MyMap //Put in this order so Inteliji won't tell me recursion isn't at the end of the function
                case Some(x) => (x foldLeft MyMap) { (m, suc) => (MyCalculation(valuation)(m, suc, currentLength + valuator(valuation)(currentlyAt, suc)) ) }
            }
        else MyMap
    }

    /**
     *
     * @param valuation The Map giving the length of all arcs
     * @param x1    Begining of the arc
     * @param x2    End of the arc
     * @return      Length of the arc (x1,x2)
     */
    def valuator(valuation : Map[Arc[V], Double])(x1:V,x2:V): Double = {
        getArcFromVertice(x1, x2) match {
            case None     => 99999999.0
            case Some(x)      => valuation.get(x) match {
                case Some(value) => value
                case None => 99999999.0
            }
        }
    }

    //Pour shortestPath: definir methode partielle prenant en parametre positionActuelle, valeurActuelle, destination, une map associant un V a un double

    /* toString-LIKE METHODS */

    /** @inheritdoc */
    override lazy val toString : String = s"({${vertices mkString ", "}}, {${arcs mkString ", "}})"

    /** Graph representation in DOT language */
    lazy val toDOTString : String = {
        "strict graph {\n" +
        "    // Edges\n" +
        (arcs foldLeft "    ") { _ + _.toDOTString + "\n    " } + "\n" +
        "    // Vertices\n" +
        vertices.mkString("    ", "\n    ", "\n") +
        "  }\n"
      }

  }
