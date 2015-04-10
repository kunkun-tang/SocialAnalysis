package object infer {
  
  import scala.collection.mutable.HashMap

  abstract class Predicate(src1: Int, src2: Int) {

    // result is to differentiate the predicte is true or not.
    var result: Boolean = true;
  }

  // MutualFrd and MutualComm are the predicates at the left side.
  case class MutualFrd(var numMutualFrds: Int, val src1: Int, val src2: Int) extends Predicate(src1, src2)
  case class MutualComm(numMutualComms: Int, src1: Int, src2: Int) extends Predicate(src1, src2)

  // FrdPredict is the predicate at the right side.
  class FrdPredict(src1: Int, src2: Int, frdsRelation: HashMap[(Int, Int), (Boolean, Boolean)]) extends Predicate(src1, src2){
    
    def getSrc1 = src1
    def getSrc2 = src2
    def getResult = result

    def setResult(re: Boolean) = result = re

    def ifKnow = frdsRelation((src1, src2))._2
  }

  val lnOf2 = math.log(2)

  def ProbDBLPCommonFrd(numMutualFrd: Int) = if (numMutualFrd >0 ){
    val ret = 0.3545 * math.log(numMutualFrd) + 0.0279;
    if(ret >= 1.0) 1.0 else ret;
  }
  else 0.000004
  
  def ProbLJCommonFrd(numMutualFrd: Int): Double = if (numMutualFrd >0 ) {
    val ret: Double = 0.0084 * math.pow(numMutualFrd, 1.0222) 
    if(ret >= 1.0) 1.0 else ret;
  }
  else 0.000004

  // def ProbDBLPCommonComm(numMutualComm: Int) = 0.012 * math.pow(numMutualComm, 1.766)
  def ProbDBLPCommonComm(numMutualComm: Int) = if (numMutualComm >0 ) {
    val ret = 0.012 * math.pow(numMutualComm, 1.466) + 0.5
    if(ret >= 1.0) 1.0 else ret;
  }
  else 0.000004


  def ProbLJCommonComm(numMutualComm: Int) = 0.0061 * math.pow(numMutualComm, 1.1498)

  import com.typesafe.config.ConfigFactory
  val conf = ConfigFactory.load
  val datasetName = conf.getString("DataSetName");

  val probCommonFrd: Int => Double = if (datasetName == "DBLP") ProbDBLPCommonFrd else ProbLJCommonFrd
  val probCommonComm: Int => Double = if (datasetName == "DBLP") ProbDBLPCommonComm else ProbLJCommonComm

  // abstract class Clause(pred1: Predicate, pred2: Predicate, var n: Int) {
  //   // result is to differentiate the clause is true or not.
  //   def result(): Boolean

  //   def setN(changeN: Int) = n = changeN
  // }

  trait Clause{
    def result: Boolean
    def setN(changeN: Int): Unit
    def getPred2: FrdPredict
    def getN: Int
  }

  class FrdClause(pred1: Predicate, pred2: FrdPredict, var n: Int) extends Clause{
    
    def result: Boolean = if(probCommonFrd(n)-0.5 < 0.5){
      if (pred1.result == true && pred2.ifKnow == true) false else true
    }
    else{
      if (pred1.result == true && pred2.ifKnow == false) false else true
    }

    def setN(changeN: Int) = n = changeN
    def getN = n
    def getPred2 = pred2
  }

  class CommClause( pred1: Predicate, pred2: FrdPredict, var n: Int) extends Clause{

    def result: Boolean = if(probCommonComm(n) < 0.5){
      if (pred1.result == true && pred2.ifKnow == true) false else true
    }
    else{
      if (pred1.result == true && pred2.ifKnow == false) false else true
    }

    def setN(changeN: Int) = n = changeN
    def getN = n
    def getPred2 = pred2
  }

  /*
   * pred1 will be the mutualFrd, and pred2 is will be mutualComm
   */
  class HybridClause( pred1: Predicate, pred2: Predicate, pred3: FrdPredict, var nFrd: Int, var nComm: Int) extends Clause{

    def result: Boolean = {
      if(probCommonFrd(nFrd) < 0.5 && probCommonComm(nComm)< 0.5){
        if (pred3.ifKnow == false) true else false
      }
      else{
        if (pred3.ifKnow == true) true else false
      }
    }

    def setN(changeN: Int) = nFrd = changeN
    def getN = nFrd
    def getPred2 = pred3
  }


  /*
   * if the prob is larger than 0.5, the weight has to be inverse computed.
   * if p is too big, bigger than 1, then set it to 0.99.
   */
  def computeWeight(p: Double) = {

    var w = 0.0; var prob = p;
    if(prob >= 1) prob = 0.99
    if (prob < 0.5) w = math.log((1 - prob) / prob)
    else w = math.log(prob / (1 - prob))

    1 - math.pow(math.E, -1 * w)
  }

  def computeWeightFrd(num: Int) = computeWeight(probCommonFrd(num))
  def computeWeightComm(num: Int) = computeWeight(probCommonComm(num))

}
