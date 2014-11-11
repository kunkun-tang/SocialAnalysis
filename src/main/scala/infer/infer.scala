package infer 

import com.typesafe.config.ConfigFactory
import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map
import scala.collection.mutable.HashSet
import scala.util.Random
import scala.math
import probabilitymonad.Distribution._

object MCSAT{

  val rand = new Random(System.currentTimeMillis())
  var frdsMapGlobal = Map[Int, ArrayBuffer[Int]]()

  def ifTwoPersonKnow(src: Int, dest: Int) = frdsMapGlobal(src).contains(dest)

  def findNumMutualFrds(src1: Int, src2: Int) = {
    val frds1 = frdsMapGlobal(src1);
    val frds2 = frdsMapGlobal(src2);
    frds1.toSet.intersect(frds2.toSet).size
  }

  def flip(pred: Predicate) = pred.result = !pred.result

  def genRandomBoolean() = if(rand.nextDouble()<0.5) true else false
  	/*
  	 * iterate over the clause arrays, if the predictArr could change its results,
  	 * assign a boolean to the predicate.
  	 */
		def assignBin(clausesArr: ArrayBuffer[Clause]) = {
		  for( clau <- clausesArr ){
		  	if(clau.pred2.changeEnable == false) clau.pred2.result = genRandomBoolean()
		}
  }


  def apply(frdsMap: Map[Int, ArrayBuffer[Int]], commMap: Map[Int, ArrayBuffer[Int]])(src1: Int, src2: Int) = {
  
    frdsMapGlobal = frdsMap
    val conf = ConfigFactory.load

    /*
     * In the first time, we need to give random boolean value to all friends pairs.
     */
    val frdsArr = new ArrayBuffer[FrdPredict]();

    for( (i1, i2)<- frdsMap; (j1, j2)<- frdsMap; if(i1<j1) ){
      if(ifTwoPersonKnow(i1,j1))
      	frdsArr.append(FrdPredict(i1, j1, true));
      else
        frdsArr.append(FrdPredict(i1, j1, genRandomBoolean))
    }

    /* 
     * The MC-SAT sample loop starts from 1 to a sample upper limit.
     */
    val probList = List(0.2, 0.3, 0.4);
    for(sampleNum <- 1 to conf.getInt("MCSATSampleNum")){

	    val clausesArr = new ArrayBuffer[Clause]();
	    for(frd <- frdsArr){
    		val num = findNumMutualFrds(frd.src1, frd.src2);

    		if(num > 0 && ifTwoPersonKnow(frd.src1, frd.src2) == false && rand.nextDouble() < computeWeight( probList(num)) ){
    			val aClause = new Clause(MutualFrd(num, frd.src1, frd.src2), frd);
    			clausesArr.append(aClause);
    		}
	    }

      /*
       * After the clausesArr is selected, we then let the arrays run on walkSAT procedure.
       */
	    walkSAT(clausesArr);
  	}
  }


  def walkSAT(clausesArr: ArrayBuffer[Clause]) = {
    assignBin(clausesArr);
    var wrongClauses = clausesArr.filter{ clau => clau.result == false}

    /*
     * WalkSat part
     */
    wrongClauses.foreach{ wc => flip(wc.pred2)};
  }
}
