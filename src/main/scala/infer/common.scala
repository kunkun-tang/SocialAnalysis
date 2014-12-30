package object infer{

	abstract class Predicate(src1: Int, src2: Int){
  	// result is to differentiate the clause is true or not.
  	var result: Boolean = false;
  }

  case class MutualFrd(numMutualFrds: Int, src1: Int, src2: Int) extends Predicate(src1, src2)
  case class FrdPredict(src1: Int, src2: Int, changeEnable: Boolean) extends Predicate(src1, src2)

  case class MutualComm(numMutualComms: Int, src1: Int, src2: Int) extends Predicate(src1, src2)

  val probList = List(0.1, 0.2, 0.25, 0.25, 0.3, 0.3, 0.4, 0.45);
	class Clause(var pred1: Predicate, var pred2: FrdPredict, val n: Int){
  	// result is to differentiate the clause is true or not.
  	def result(): Boolean = {
      if(n >= probList.size){
        if(pred1.result == true && pred2.result == false) false else true
      }
      else{
        if(probList(n)<0.5) if(pred1.result == true && pred2.result == true) false else true
        else if(pred1.result == true && pred2.result == false) false else true
      }
    }
  }


	import scala.math;
  def computeWeight(p: Double) = {
  	val w = math.log((1-p)/p)
  	1-math.pow(math.E, -1*w)
  }

  def computeWeightBasedonNumber(num: Int) = 
    if(num< probList.size) computeWeight(probList(num))
    else 0.9
}