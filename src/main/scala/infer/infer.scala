package infer

import com.typesafe.config.ConfigFactory
import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import scala.util.Random
import scala.math
import probabilitymonad.Distribution._

object MCSAT {

  val rand = new Random(System.currentTimeMillis())
  var frdsMapGlobal: Map[Int, ArrayBuffer[Int]] = null
  var frdsMapLocal: Map[Int, ArrayBuffer[Int]] = null
  var frdsRelation: HashMap[(Int, Int), (Boolean, Boolean)] = null;

  var commsMapGlobal = Map[Int, ArrayBuffer[Int]]()

  def ifTwoPersonKnow(src: Int, dest: Int) = frdsMapGlobal(src).contains(dest)

  def findNumMutualFrdsLocal(src1: Int, src2: Int) = {
    var ret = 1;
    if(frdsMapLocal.contains(src1) == false || frdsMapLocal(src1) == null) ret = 0;
    if(frdsMapLocal.contains(src2) == false || frdsMapLocal(src2) == null) ret = 0;
    if(ret == 0) 0
    else{
      val frds1 = frdsMapLocal(src1);
      val frds2 = frdsMapLocal(src2);
      frds1.toSet.intersect(frds2.toSet).size
    }
  }

  def findNumMutualComms(src1: Int, src2: Int): Int = {
    var ret = 1;
    if(commsMapGlobal.contains(src1) == false || commsMapGlobal(src1) == null) ret = 0;
    if(commsMapGlobal.contains(src2) == false || commsMapGlobal(src2) == null) ret = 0;
    if( ret == 0) 0
    else{
      val comm1 = commsMapGlobal(src1);
      val comm2 = commsMapGlobal(src2);
      // println(comm1 + " " + comm2)
      comm1.toSet.intersect(comm2.toSet).size
    }
  }

  def genRandomBoolean() = if (rand.nextDouble() < 0.5) true else false

  /*
  	 * iterate over the frds pair, if the frds pair is allowed to change its results,
  	 * assign a boolean to the predicate.
  	 */
  def assignBin(frdsRelation: HashMap[(Int, Int), (Boolean, Boolean)]) = {
    for( (k,v) <- frdsRelation ){
      if(v._1 == true){ 
        frdsRelation.update(k,  (true, genRandomBoolean()) );
      }
    }
  }

  var testFrd1 = -1; var testFrd2 = -1;
  /*
   * frdsMap is (userID, ArrayBuffer(userID_63, userID_22, ...)) 
        ArrayBuffer is userID's friends list.
   * commsMap is (userID, ArrayBuffer(commID_23, commID_45, ...)) 
        ArrayBuffer is userID's community list.
   */
  def apply(frdsMap: Map[Int, ArrayBuffer[Int]], commMap: Map[Int, ArrayBuffer[Int]])(src1: Int, src2: Int) = {

    frdsMapGlobal = frdsMap
    commsMapGlobal = commMap
    frdsRelation = new scala.collection.mutable.HashMap[(Int, Int), (Boolean, Boolean)]();
    val conf = ConfigFactory.load
    println("src1 ="+src1 + " src2="+src2)

    /*
     * frdsAr can be regarded as a collections of frds pair.
     * frdsRelation is the big table for every two src node pair,
     * (frd1, frd2) -> (if the two are changeable, if they know each other)
     */

    for ((i1, i2) <- frdsMap; (j1, j2) <- frdsMap; if (i1 < j1)) {
      if (ifTwoPersonKnow(i1, j1))
        frdsRelation += (i1, j1) -> (false, true)
      else
        frdsRelation += (i1, j1) -> (true, genRandomBoolean)
    }
    frdsMapLocal = frdsMapGlobal.clone();


    var first: Boolean= true;
    /*
     * The MC-SAT sample loop starts from 1 to a sample upper limit.
     */
    var counter = 0
    for (sampleNum <- 1 to conf.getInt("MCSATSampleNum")) {

    	//re new ArraBuffer and HashMap
      val clausesArr = new ArrayBuffer[Clause]();
      val clausesMap = new HashMap[(Int, Int), (Clause, Clause)]();

      /**
       * The iteration begins from finding the mutual frd number of two persons.
       * (frd1, frd2, +n)->(frd1, frd2) has to be satisfied in order to be 
       * put in to Constraint set.
       * Besides, it must satisfy the probability that the clause be into the Constraint set.
       */
      val allLength = frdsMapLocal.foldLeft(0)( (B, kv) => B + kv._2.length);

      for((k,v) <- frdsRelation){
        val num = findNumMutualFrdsLocal(k._1, k._2);
        // if(num > 3 && k._1 == src1 && k._2 == src2 && first == true){
        //   testFrd1 = k._1; testFrd2 = k._2; first = false;
        // }
        val probFromFrdCurve = probCommonFrd(num);
        // if(k._1 == testFrd1 && k._2 == testFrd2){
        //   println("numComm=" + findNumMutualComms(k._1, k._2) + " numFrd=" + num + " boolean=" + v._2 + " weight="+computeWeightFrd(num))
        // }
        if (v._1 == true && v._2==true && num > 0 && rand.nextDouble() < computeWeightFrd(num)) {
          // println("num =" + num + " access");
          if( (probFromFrdCurve >= 0.5 && v._2 == true) || 
              (probFromFrdCurve < 0.5 && v._2 == false) ){
            val pred1 = MutualFrd(num, k._1, k._2);
            pred1.result = true;
            val pred2 = new FrdPredict(k._1, k._2, frdsRelation);
            pred2.result = v._2;
            // println("mutualFrds: " + num + "  " + computeWeightFrd(num) + " probFromFrdCurve = " + probFromFrdCurve);
            val aClause = new FrdClause(pred1, pred2, num);
            clausesArr.append(aClause);
            clausesMap += (k._1, k._2) -> (aClause, null)
          }
        }

        // val numComm = findNumMutualComms(k._1, k._2);
        // val probFromCommCurve = probCommonComm(numComm);
        // if (v._1 == true && num > 0 && rand.nextDouble() < computeWeightComm(numComm)) {
        //   if( (probFromCommCurve >= 0.5 && v._2 == true) || 
        //       (probFromCommCurve < 0.5 && v._2 == false) ){

        //     val pred1 = MutualComm(numComm, k._1, k._2);
        //     pred1.result = true;
        //     val pred2 = new FrdPredict(k._1, k._2, frdsRelation);
        //     pred2.result = v._2;
        //     val aClause = new CommClause(pred1, pred2, num);
        //     clausesArr.append(aClause);
        //     if(clausesMap.contains( (k._1, k._2) ) == false) 
        //       clausesMap += (k._1, k._2) -> (null, aClause)
        //     else{
        //       val (v1, v2) = clausesMap( (k._1, k._2) );
        //       clausesMap += (k._1, k._2) -> (v1, aClause);
        //     }
        //   }
        // }
      }

      /*
       * After the clausesArr and clausesMap is selected, we then let the arrays run on walkSAT procedure.
       */
      // println("before access clausesArr size = " + clausesArr.size);
      walkSAT(clausesArr, clausesMap);

      // if(testFrd1 != -1)
      //   println("after walkSAT the value=" + frdsRelation((testFrd1, testFrd2)) );
      if(frdsRelation((src1,src2))._2 == true ) counter += 1
    }
    first = true;
    testFrd1 = -1; testFrd2 = -1;
    println("If the the two people know each other = " + counter.toFloat/conf.getInt("MCSATSampleNum"))
    val numMutualFrds = findNumMutualFrdsLocal(src1, src2);
    if(numMutualFrds == 0)
      (counter.toFloat/conf.getInt("MCSATSampleNum"), 0.0);
    else 
      (counter.toFloat/conf.getInt("MCSATSampleNum"), probCommonFrd(numMutualFrds));
  }

  def walkSAT(clausesArr: ArrayBuffer[Clause], clausesMap: HashMap[(Int, Int), (Clause, Clause)]) = {
  	// assign randome values to all uncertain nodes
    assignBin(frdsRelation);

    // if(testFrd1 != -1)
    //   println("in walkSAT the value=" + frdsRelation((testFrd1, testFrd2)) );
    /*
     * Use frdsMapLocal to try solving salkSAT problem.
     * For current information in clausesArr, we update the situations in clausesArr.
     * For examples, if (frd1, frd2) is true now in clausesArr, we make it an edge (frd1, frd2)
     * in frdsMapLocal as well.
     */
    frdsMapLocal = frdsMapGlobal.clone();
    var wrongClauses = clausesArr.filter { clau => clau.result == false }

    wrongClauses.foreach { wc =>
      {
        if (wc.result == false) {
          // println("flip MutualFrd: " + wc.getN + " prd2 = " + wc.getPred2.ifKnow)
          flip(wc.getPred2)
          // println("after flip prd2 = " + wc.getPred2.ifKnow + " clause result =  " + wc.result )
        }
      }
    }
    val wrongClausesNum = clausesArr.filter { clau => clau.result == false }.size;
    println("After walkSAT wrongClausesNum = " + wrongClausesNum);
  }

	/* @return:
	 * 0 means that frdPredict is not changed. (Deprecated, since I only put changeable clauses here)
	 * 1 means that adding an edge
	 * -1 means that removing an age.
	 *
	 * In this function, src1 always < src2
	 */
  def flip(frdPredict: FrdPredict) = {

		val (src1, src2) = (frdPredict.getSrc1, frdPredict.getSrc2);

    // if(src1 == testFrd1 && src2 == testFrd2){
    //   println("ifknow = " + frdPredict.ifKnow);
    // }

		if(frdPredict.ifKnow == true){
			frdPredict.setResult(false);
      if(src1 < src2) frdsRelation.update( (src1, src2), (true, false) );
      else frdsRelation.update((src2, src1), (true, false) );
			-1
		}
		else{
      frdPredict.setResult(true);
      if(src1 < src2) frdsRelation.update( (src1, src2), (true, true) );
      else frdsRelation.update((src2, src1), (true, true) );
			1
		}
	}

}
