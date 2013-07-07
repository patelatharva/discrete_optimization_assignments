package knapsack_solution

import scala.collection.mutable.PriorityQueue
import scala.annotation.tailrec
import scala.io.Source
object Solver {

  def main(args: Array[String]) {
    val sampleProblem = List((1, 2, 0), (3, 9, 3), (1, 4, 1), (2, 5, 2))
    val capacity = 3

    var filename: String = null;
    for (arg <- args) {
      if (arg.startsWith("-file=")) {
        filename = arg.substring(6);
      }
    }
    if (filename == null) {
      return ;
    }
    val lines = Source.fromFile(filename).getLines()

    if (lines.hasNext) {
      val firstLine = lines.next
      val words = firstLine.split("\\s+");
      val nString = words(0)
      val n = Integer.parseInt(nString)
      
      val kString = words(1)
      val k = Integer.parseInt(kString)
      var things = List[(Int, Int, Int)]()
      //println("n string: "+nString)
      //println("n :"+n)
      //println("k string: "+kString)
      //println("k: "+k)
      for (i <- 0 to n - 1) {

        if (lines.hasNext) {
          val valueAndWeightString = lines.next.trim
          val words = valueAndWeightString.split("\\s+")
          val valueString = words(0)
          val weightString = words(1)
          if (weightString != null && !weightString.isEmpty && valueString != null && !valueString.isEmpty) {
            val weight = Integer.parseInt(weightString)
            val value = Integer.parseInt(valueString)
            things = things :+ (weight, value, i)
          } else {
            //println(valueAndWeightString)
          }

        }

      }
      //println(things.length)
      val solution = getBestSolution(things, k)
      println(solution._1 + " " + 0)
      solution._2.foreach(x => print(x + " "))
    }

  }
  
  def getProblemFromInput(inputData: String) {

  }
  def getBestSolution(things: List[(Int, Int, Int)], capacity: Int): (Int, List[Int]) = {
    var maxObjValue = 0
    var bestDecisionTillNow = List[Int]()
    def ValuePerWeightCompare = new Ordering[(Int, Int, Double, Int)] {
      def compare(x: (Int, Int, Double, Int), y: (Int, Int, Double, Int)): Int = {
        x._3.compare(y._3)
      }
    }

    //    def addElementToQueue(toBeFilledUpQueue: PriorityQueue[(Int, Int, Double,Int)], element: (Int, Int, Int)) {
    //      toBeFilledUpQueue + new Tuple4(element._1, element._2, element._2 / element._1, element._3)
    //    }
    val queue: PriorityQueue[(Int, Int, Double, Int)] = new PriorityQueue[(Int, Int, Double, Int)]()(ValuePerWeightCompare)
    val valuePerWeightSortedQueue = things.foldLeft(queue)((toBeFilledUpQueue, element) => {
      toBeFilledUpQueue += ((element._1, element._2, element._2.toDouble / element._1.toDouble, element._3))
    })
    def chooseFromTheRemainingThings(remainingCapacity: Int, remainingThings: List[(Int, Int, Int)], ongoingDecision: List[Int], accValue: Int, valuePerWeightSorted: PriorityQueue[(Int, Int, Double, Int)]) {

      def getOptimisticValue(): Double = {

        def fillWithMostValueAddingStuff(valuePerWeightSortedArg: PriorityQueue[(Int, Int, Double, Int)], remainingCapacity: Int, accOptimisticValue: Double): Double = {
          if (remainingCapacity > 0 && !valuePerWeightSortedArg.isEmpty) {
            val x = valuePerWeightSortedArg.head
            if (x._1 <= remainingCapacity) {
              val copyOfValuePerWeightSorted = valuePerWeightSortedArg.clone()
              copyOfValuePerWeightSorted.dequeue()
              fillWithMostValueAddingStuff(copyOfValuePerWeightSorted, remainingCapacity - x._1, accOptimisticValue + x._2)
            } else
              (accOptimisticValue + remainingCapacity * x._3).toDouble
          } else {
            0.0 + accOptimisticValue
          }
        }
        fillWithMostValueAddingStuff(valuePerWeightSorted, remainingCapacity, accValue)
      }

      if (remainingThings.isEmpty) {
//    	//println("remainingThings empty")
//    	//println("accValue: "+accValue)
        if (accValue > maxObjValue) {
          maxObjValue = accValue
          bestDecisionTillNow = ongoingDecision
          
        }
      } else {
        //println("remaining things is not empty")
        if (getOptimisticValue() > maxObjValue) {
          //println("Optimistic value:"+getOptimisticValue()+" > maxObjValue:"+ maxObjValue )
          val x = remainingThings.head
          if (x._1 > remainingCapacity)
            chooseFromTheRemainingThings(remainingCapacity, remainingThings.tail, ongoingDecision :+ 0, accValue, valuePerWeightSorted.clone().filterNot((a) => a._4 == x._3))
          else {
            chooseFromTheRemainingThings(remainingCapacity - x._1, remainingThings.tail, ongoingDecision :+ 1, accValue + x._2, valuePerWeightSorted.clone().filterNot((a) => a._4 == x._3))
            chooseFromTheRemainingThings(remainingCapacity, remainingThings.tail, ongoingDecision :+ 0, accValue, valuePerWeightSorted.clone().filterNot((a) => a._4 == x._3))
          }
        }else {
          //println ("Optimistic value:"+getOptimisticValue()+" < maxObjValue:"+ maxObjValue)
        }
      }
      //
    }

    chooseFromTheRemainingThings(capacity, things, List[Int](), 0, valuePerWeightSortedQueue)
    (maxObjValue, bestDecisionTillNow)
  }

}