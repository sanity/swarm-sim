import scala.collection.mutable.HashMap
import scala.collection.mutable.Set
import scala.collection.mutable.HashSet
import scala.util.Random

object Main {
  def main(args : Array[String]) : Unit = {
	  val sb = new HashMap[Int, Set[Int]]()
	  val random = new Random()
	  for (p <- 0 until 16) {
	 	  val hs = new HashSet[Int]()
	 	  for (k <- 0 until 6) {
	 	 	  var a = p + random.nextInt(4) - 2
	 	 	  if (a < 0) a = 0
	 	 	  if (a > 15) a = 15
	 	 	  if (a != p) hs.add(a)
	 	  }
	 	  val b = random.nextInt(16)
	 	  if (b != p) hs.add(b)
	 	 sb.put(p, hs)
	  }
	  val qb = new QuadBlock(sb, 2)
	  for (x <- 0 until 3) {
	 	  print(qb.crossCount._1+"\t")
	 	  printSB(sb)
	 	  println()
	 	 
	 	  // println(qb.crossCount._1)
	 	  qb.optimize
	  }
  }
  
  def printSB(sb : HashMap[Int, Set[Int]]) = {
	  var x=0
	  while (x!= -1) {
	 	  sb.get(x) match {
	 	 	  case Some(v) => {
	 	 	 	  print(x+":"+v+" ")
	 	 	 	  x += 1
	 	 	  }
	 	 	  case None => x = -1;
	 	  }
	  }
  }
}
