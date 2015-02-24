package object infer {

  abstract class Predicate(src1: Int, src2: Int) {

    // result is to differentiate the clause is true or not.
    var result: Boolean = false;
  }

  case class MutualFrd(var numMutualFrds: Int, val src1: Int, val src2: Int) extends Predicate(src1, src2)
  case class FrdPredict(src1: Int, src2: Int, changeEnable: Boolean) extends Predicate(src1, src2)

  case class MutualComm(numMutualComms: Int, src1: Int, src2: Int) extends Predicate(src1, src2)

  val lnOf2 = math.log(2)

  def ProbDBLPCommonFrd(numMutualFrd: Int) = 0.3545 * math.log(numMutualFrd) / lnOf2 + 0.0279
  def ProbLJCommonFrd(numMutualFrd: Int) = 0.0084 * math.pow(numMutualFrd, 1.0222)

  def ProbDBLPCommonComm(numMutualComm: Int) = 0.0012 * math.pow(numMutualComm, 1.766)
  def ProbLJCommonComm(numMutualComm: Int) = 0.0061 * math.pow(numMutualComm, 1.1498)

  import com.typesafe.config.ConfigFactory
  val conf = ConfigFactory.load
  val datasetName = conf.getString("DataSetName");

  val probCommonFrd: Int => Double = if (datasetName == "DBLP") ProbDBLPCommonFrd else ProbLJCommonFrd
  val probCommonComm: Int => Double = if (datasetName == "DBLP") ProbDBLPCommonComm else ProbLJCommonComm

  class Clause(var pred1: Predicate, var pred2: FrdPredict, val n: Int) {
    // result is to differentiate the clause is true or not.
    def result(): Boolean = {
      if (probCommonFrd(n) < 0.5) if (pred1.result == true && pred2.result == true) false else true
      else if (pred1.result == true && pred2.result == false) false else true
    }
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

  def computeWeightBasedonNumber(num: Int) = computeWeight(probCommonFrd(num))

}