package graph_coloring

import scala.io.Source
import scala.Array._
object Solver {

  def main(args: Array[String]): Unit = {
    //print the input data
    var filename: String = null;
    for (arg <- args) {
      if (arg.startsWith("-file=")) {
        filename = arg.substring(6);
      }
    }
    if (filename == null) {
      println("filename was null")
      return ;
    }
    val lines = Source.fromFile(filename).getLines()

    if (lines.hasNext) {
      val firstLine = lines.next
      val words = firstLine.split("\\s+");
      val nString = words(0)
      val n = Integer.parseInt(nString)
      
      val eString = words(1)
      val e = Integer.parseInt(eString)
      var edges = List[(Int, Int)]()
      for (i <- 0 to e - 1) {

        if (lines.hasNext) {
          val node2AndNode2String = lines.next.trim
          val words = node2AndNode2String.split("\\s+")
          val node1String = words(0)
          val node2String = words(1)
          if (node1String != null && !node1String.isEmpty && node2String != null && !node2String.isEmpty) {
            val node1 = Integer.parseInt(node1String)
            val node2 = Integer.parseInt(node2String)
            edges = edges :+ (node1, node2)
          } else {
            //println(valueAndWeightString)
          }

        }

      }
      //val edges = List[(Int, Int)]((0, 1), (0, 2), (1, 2), (1, 3),(0,3));
      val solution = getBestSolution(edges, n)
      println(solution._2+" "+1);
      solution._1.foreach(x => print(x+" "));
      
     
      //println(things.length)
      //      val solution = getBestSolution(things, k)
      //      println(solution._1 + " " + 0)
      //      solution._2.foreach(x => print(x + " "))

      //    }
    }
  }
  def getBestSolution(edges: List[(Int, Int)], numOfNodes: Int): (List[Int], Int) = {
    val adjNodesIndex = ofDim[List[Int]](numOfNodes);
    var minNumOfColorsTillNow = Int.MaxValue;
    var bestColorDecisionsTillNow = List[Int]();
    for (edge <- edges) {
      if (adjNodesIndex(edge._1) == null) {
        adjNodesIndex(edge._1) = List[Int]();
      }
      adjNodesIndex(edge._1) = (adjNodesIndex(edge._1)) :+ (edge._2)
      if (adjNodesIndex(edge._2) == null) {
        adjNodesIndex(edge._2) = List[Int]();
      }
      adjNodesIndex(edge._2) = (adjNodesIndex(edge._2)) :+ (edge._1)
    }
    val notPossibleColorsTable = ofDim[List[Int]](numOfNodes);
    val searchSpace = (notPossibleColorsTable, 1); //(notPossibleColors index, max number of colors to be used)
    def takeDecisionForNodeColor(nodeIndex: Int, searchSpace: (Array[List[Int]], Int), colorDecisions: List[Int]) {
      val optionsForNodeColor = if (searchSpace._1(nodeIndex) != null && !searchSpace._1(nodeIndex).isEmpty) {
        (0 to (searchSpace._2 - 1)) filterNot (searchSpace._1(nodeIndex) contains)
      } else {
        (0 to (searchSpace._2 - 1))
      }

      def evaluateOption(color: Int, searchSpace: (Array[List[Int]], Int)) {
        def checkConstraints(): (Boolean, (Array[List[Int]], Int), List[Int]) = {
          //check for feasibility
          //checks whether we are working on a solution worse then one which is already found then drop it
          if (searchSpace._2 > minNumOfColorsTillNow) {
            (false, null, null);
          } else {
            //mark this color as a not possible color for adj nodes
            val prunedSearchSpace = (searchSpace._1.clone, searchSpace._2)
            for (adjNode <- adjNodesIndex(nodeIndex)) {
              if (prunedSearchSpace._1(adjNode) == null) {
                prunedSearchSpace._1(adjNode) = List[Int]();
              }
              prunedSearchSpace._1(adjNode) = (prunedSearchSpace._1(adjNode)) :+ color
            }
            (true, prunedSearchSpace, colorDecisions :+ color)
          }
        }
        val (isFeasible, prunedSearchSpace, updatedColorDecisions) = checkConstraints();
        if (isFeasible) {
          if (nodeIndex == numOfNodes - 1) {
            //we have found one solution
            //evaluate the score of this solution
            if (prunedSearchSpace._2 < minNumOfColorsTillNow) {
              minNumOfColorsTillNow = prunedSearchSpace._2;
              bestColorDecisionsTillNow = updatedColorDecisions;
            }
          } else {
            takeDecisionForNodeColor(nodeIndex + 1, prunedSearchSpace, updatedColorDecisions)
          }
        }

      }

      if (optionsForNodeColor != null && !optionsForNodeColor.isEmpty) {
        for (color <- optionsForNodeColor) {
          evaluateOption(color, searchSpace)
        }
      }
      //expand search space
      val expandedSearchSpace = (searchSpace._1, searchSpace._2 + 1);
      evaluateOption(searchSpace._2, expandedSearchSpace);
    }
    takeDecisionForNodeColor(0, searchSpace, List[Int]())
    (bestColorDecisionsTillNow, minNumOfColorsTillNow);
  }

}