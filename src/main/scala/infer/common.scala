package object infer{

	abstract class Predicate(src1: Int, src2: Int){
  	// result is to differentiate the clause is true or not.
  	var result: Boolean = false;
  }

  case class MutualFrd(numMutualFrds: Int, src1: Int, src2: Int) extends Predicate(src1, src2)
  case class FrdPredict(src1: Int, src2: Int, changeEnable: Boolean) extends Predicate(src1, src2)

  case class MutualComm(numMutualComms: Int, src1: Int, src2: Int) extends Predicate(src1, src2)

	class Clause(var pred1: Predicate, var pred2: FrdPredict){
  	// result is to differentiate the clause is true or not.
  	def result: Boolean = if(pred1.result == true && pred2.result == true) false else true
  }

	import scala.math;
  def computeWeight(p: Double) = {
  	val w = math.log((1-p)/p)
  	1-math.pow(math.E, -1*w)
  }
}