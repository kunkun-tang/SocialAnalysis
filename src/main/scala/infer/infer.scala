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
  var frdsMapGlobal = Map[Int, ArrayBuffer[Int]]()
  var frdsMapLocal = Map[Int, ArrayBuffer[Int]]()
  var frdsRelation = new scala.collection.mutable.HashMap[(Int, Int), (Boolean, Boolean)]();

  var commsMapGlobal = Map[Int, ArrayBuffer[Int]]()

  def ifTwoPersonKnow(src: Int, dest: Int) = frdsMapGlobal(src).contains(dest)

  def findNumMutualFrdsLocal(src1: Int, src2: Int) = {
    val frds1 = frdsMapLocal(src1);
    val frds2 = frdsMapLocal(src2);
    frds1.toSet.intersect(frds2.toSet).size
  }

  def findNumMutualComms(src1: Int, src2: Int) = {
    val comm1 = commsMapGlobal(src1);
    val comm2 = commsMapGlobal(src2);
    comm1.toSet.intersect(comm2.toSet).size
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

  /*
   * frdsMap is (userID, ArrayBuffer(userID_63, userID_22, ...)) 
        ArrayBuffer is userID's friends list.
   * commsMap is (userID, ArrayBuffer(commID_23, commID_45, ...)) 
        ArrayBuffer is userID's community list.
   */
  def apply(frdsMap: Map[Int, ArrayBuffer[Int]], commMap: Map[Int, ArrayBuffer[Int]])(src1: Int, src2: Int) = {

    frdsMapGlobal = frdsMap
    commsMapGlobal = commMap
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

      for((k,v) <- frdsRelation){
        // looking for satisfied clauses that 
        val num = findNumMutualFrdsLocal(k._1, k._2);
        val probFromFrdCurve = probCommonFrd(num);

        if (v._1 == true && num > 0 && rand.nextDouble() < computeWeightFrd(num)) {
          if( (probFromFrdCurve >= 0.5 && v._2 == true) || 
              (probFromFrdCurve < 0.5 && v._2 == false) ){
            val pred1 = MutualFrd(num, k._1, k._2);
            pred1.result = true;
            val pred2 = FrdPredict(k._1, k._2, v._1);
            pred2.result = v._2;
            val aClause = new FrdClause(pred1, pred2, num);
            clausesArr.append(aClause);
            clausesMap += (k._1, k._2) -> (aClause, null)
          }
        }

        val numComm = findNumMutualComms(k._1, k._2);
        val probFromCommCurve = probCommonComm(numComm);
        if (v._1 == true && num > 0 && rand.nextDouble() < computeWeightComm(numComm)) {
          if( (probFromCommCurve >= 0.5 && v._2 == true) || 
              (probFromCommCurve < 0.5 && v._2 == false) ){

            val pred1 = MutualComm(numComm, k._1, k._2);
            pred1.result = true;
            val pred2 = FrdPredict(k._1, k._2, v._1);
            pred2.result = v._2;
            val aClause = new CommClause(pred1, pred2, num);
            clausesArr.append(aClause);
            if(clausesMap.contains( (k._1, k._2) ) == false) 
              clausesMap += (k._1, k._2) -> (null, aClause)
            else{
              val (v1, v2) = clausesMap( (k._1, k._2) );
              clausesMap += (k._1, k._2) -> (v1, aClause);
            }
          }
        }
      }

      /*
       * After the clausesArr and clausesMap is selected, we then let the arrays run on walkSAT procedure.
       */
      println("before access clausesArr size = " + clausesArr.size);
      walkSAT(clausesArr, clausesMap);

      if(frdsRelation((src1,src2))._2 == true ) counter += 1
    }

    println("If the the two people know each other = " + counter.toFloat/conf.getInt("MCSATSampleNum"))
    counter.toFloat/conf.getInt("MCSATSampleNum")
  }

  def walkSAT(clausesArr: ArrayBuffer[Clause], clausesMap: HashMap[(Int, Int), (Clause, Clause)]) = {
  	// assign randome values to all uncertain nodes
    assignBin(frdsRelation);

    println("access walkSAT num: " + clausesArr.length)
    /*
     * Use frdsMapLocal to try solving salkSAT problem.
     * For current information in clausesArr, we update the situations in clausesArr.
     * For examples, if (frd1, frd2) is true now in clausesArr, we make it an edge (frd1, frd2)
     * in frdsMapLocal as well.
     */
    val frdsMapLocal = frdsMapGlobal.clone();
    for( (k,v)<- frdsRelation){
    	val frd1 = k._1;
    	val frd2 = k._2;

    	if(v._1 == true && v._2 == true){
    		if(frdsMapLocal(frd1).contains(frd2) == false){
					frdsMapLocal(frd1) += frd2
          if(frdsMapLocal(frd2) == null) frdsMapLocal(frd2) = ArrayBuffer[Int]()
					frdsMapLocal(frd2) += frd1
    		}
    	}

    	if(v._1 == true && v._2 == false){
    		if(frdsMapLocal(frd1) != null && frdsMapLocal(frd1).contains(frd2) == true){
					frdsMapLocal(frd1).remove(frdsMapLocal(frd1).indexOf(frd2))
          if(frdsMapLocal(frd2) != null && frdsMapLocal(frd2).contains(frd1) == true)
					 frdsMapLocal(frd2).remove(frdsMapLocal(frd2).indexOf(frd1))
    		}
    	}
    }


    var wrongClauses = clausesArr.filter { clau => clau.result == false }
    var wrongClausesNum = wrongClauses.length;

    // if there still has clauses which is false.
    var iterWalkSAT = 0
    while (wrongClausesNum > 0 && iterWalkSAT < conf.getInt("maxWalkSAT")){

      val selectedClause = wrongClauses(rand.nextInt(wrongClauses.length));
      val (frd1, frd2) = (selectedClause.getPred2.src1, selectedClause.getPred2.src2)
      
			/*
       *---flip FrdPredict
			 * it returns a value that if the whole graph needs to add to an edge, or remove an edge,
			 * or we don't need to do anything.
			 */
      val returnVal = flip(selectedClause.getPred2)

      /*
       * Adjust other clauses, since one edge is missing or added
       * We let the (frd1. frd2) is the edge affected. j is frd1's friend.
       * if (frd1, frd2) is deleted after flip, the mutual frds number between j and frd1
       * might be changed.
       */
	    for(j<-frdsMapLocal(frd1); if j != frd1){
	    	adjustFrdClause(clausesMap, frd2, frd1, j, returnVal);
	    }

	    for(j<-frdsMapLocal(frd2); if j != frd2){
	    	adjustFrdClause(clausesMap, frd1, frd2, j, returnVal);
	    }

	    wrongClauses = clausesArr.filter { clau => clau.result == false }
	    wrongClausesNum = wrongClauses.length;
	    if(iterWalkSAT % 2000 == 0)
      	println("iterWalkSAT=" + iterWalkSAT + "  wrongClausesNum = " + wrongClausesNum);
      iterWalkSAT += 1;
    }
  }

  // In this function, we focus on the mutual frds number betweeb frdsrc and j.
  // Because comm relationship doesn't rely on the the mutual frds number, it is not affected.
  def adjustFrdClause(clausesMap: HashMap[(Int, Int), (Clause, Clause)], frdTarget:Int, frdsrc: Int, j: Int, retVal: Int)={
	  var cl: Clause = null;
  	if(j> frdsrc){
  		if(clausesMap.contains((frdsrc, j)))
  			cl= clausesMap((frdsrc, j))._1;
  	}
  	else{
  		if(clausesMap.contains((j, frdsrc)))
  			cl= clausesMap((j, frdsrc))._1;
  	}
  	if(cl != null){
  		if(retVal < 0 && frdsMapLocal(j).contains(frdTarget)){
  			cl.setN(cl.getN-1)
  		}
   		if(retVal > 0 && frdsMapLocal(j).contains(frdTarget)){
  			cl.setN(cl.getN+1)
  		}
  	}
  }

	/* @return:
	 * 0 means that frdPredict is not changed.
	 * 1 means that adding an edge
	 * -1 means that removing an age.
	 *
	 * In this function, src1 always < src2
	 */
  def flip(frdPredict: FrdPredict) = {

		val (src1, src2) = (frdPredict.src1, frdPredict.src2)
		if(frdPredict.result == true){
			if(frdsMapLocal(src1) != null && frdsMapLocal(src1).contains(src2)){
				// println("src1 frds : " + frdsMapGlobal(src1))
				// println("src2 frds : " + frdsMapGlobal(src2))
				frdsMapLocal(src1).remove(frdsMapLocal(src1).indexOf(src2))
        if(frdsMapLocal(src2)!= null && frdsMapLocal(src2).contains(src1))
				  frdsMapLocal(src2).remove(frdsMapLocal(src2).indexOf(src1))
			}
			frdPredict.result = false;
			-1
		}
		else{
      if(frdsMapLocal(src1) == null) frdsMapLocal(src1) = ArrayBuffer[Int]()
      if(frdsMapLocal(src2) == null) frdsMapLocal(src2) = ArrayBuffer[Int]()

      if(frdsMapLocal(src1).contains(src2) == false) frdsMapLocal(src1) += src2
      if(frdsMapLocal(src2).contains(src1) == false) frdsMapLocal(src2) += src1

			frdPredict.result = true;
			1
		}
	}

}
