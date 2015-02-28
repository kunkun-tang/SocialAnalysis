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

  var commsMapGlobal = Map[Int, ArrayBuffer[Int]]()

  def ifTwoPersonKnow(src: Int, dest: Int) = frdsMapGlobal(src).contains(dest)

  def findNumMutualFrds(src1: Int, src2: Int) = {
    val frds1 = frdsMapGlobal(src1);
    val frds2 = frdsMapGlobal(src2);
    frds1.toSet.intersect(frds2.toSet).size
  }

  def findNumMutualFrdsLocal(src1: Int, src2: Int) = {
    val frds1 = frdsMapLocal(src1);
    val frds2 = frdsMapLocal(src2);
    frds1.toSet.intersect(frds2.toSet).size
  }

  def findNumMutualComms(src1: Int, src2: Int) = {
    val comm1 = frdsMapGlobal(src1);
    val comm2 = frdsMapGlobal(src2);
    comm1.toSet.intersect(comm2.toSet).size
  }

  def flip(pred: Predicate) = pred.result = !pred.result

  def genRandomBoolean() = if (rand.nextDouble() < 0.5) true else false
  /*
  	 * iterate over the clause arrays, if the predictArr could change its results,
  	 * assign a boolean to the predicate.
  	 */
  def assignBin(clausesArr: ArrayBuffer[Clause]) = {
    for (clau <- clausesArr) {
      if (clau.pred2.changeEnable == true) {
        clau.pred2.result = genRandomBoolean()
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
     */
    val frdsArr = new ArrayBuffer[FrdPredict]();
    var queryFrdPredict: FrdPredict = null;
    var count1 =0; var count2 = 0;
    for ((i1, i2) <- frdsMap; (j1, j2) <- frdsMap; if (i1 < j1)) {

      if (ifTwoPersonKnow(i1, j1)) {
        val frdPredict = FrdPredict(i1, j1, false);
        frdPredict.result = true
        frdsArr.append(frdPredict);
        count1 += 1
      } else {
        val frdPredict = FrdPredict(i1, j1, true);
        frdPredict.result = genRandomBoolean
        frdsArr.append(frdPredict)
        count2 += 1
      }
    }

    /*
     * Before MC-SAT run repeatedly, we need to give random boolean value to all friends pairs.
     * Then run WalkSAT.
     *
     * Frist, we select pairs from original pair of any two persons.
     * ClausesArr is clauses set, which includes satisfied clauses.
     * clausesMap is clauses HashMap
     * (src1, src2) ->  (muturalFrdClause, mutualCommClause)
     */
    var clausesArr = new ArrayBuffer[Clause]();
    var clausesMap = new HashMap[(Int, Int), (Clause, Clause)]();

    // Before MC-SAT loop starts, all clauses must be put into walkSAT.
    for (frdPredict <- frdsArr) {
      val num = findNumMutualFrds(frdPredict.src1, frdPredict.src2);
      // if(num>0)
      //   println(frdPredict.src1 + " " + frdPredict.src2 + " " + num + " "+ probCommonFrd(num) + "  " + computeWeightBasedonNumber(num))

      // if (num > 0 && rand.nextDouble() < computeWeightBasedonNumber(num)) {
      // 	if( (probCommonFrd(num) >= 0.5 && frdPredict.result == true) || 
      // 			(probCommonFrd(num) < 0.5 && frdPredict.result == false) ){
	        val pred1 = MutualFrd(num, frdPredict.src1, frdPredict.src2);
	        pred1.result = true;
	        val aClause = new Clause(pred1, frdPredict, num);
	        clausesArr.append(aClause);
	        clausesMap += (frdPredict.src1, frdPredict.src2) -> (aClause, null)
      // 	}
      // }
    }

    println("before access clausesArr size = " + clausesArr.size);
    //After WalkSAT, we back up the frdsMapLocal.
    walkSAT(clausesArr, clausesMap);
    frdsMapGlobal = frdsMapLocal

    /*
     * The MC-SAT sample loop starts from 1 to a sample upper limit.
     */
    var counter = 0
    for (sampleNum <- 1 to conf.getInt("MCSATSampleNum")) {

    	//re new ArraBuffer and HashMap
      clausesArr = new ArrayBuffer[Clause]();
      clausesMap = new HashMap[(Int, Int), (Clause, Clause)]();

      /**
       * The iteration begins from finding the mutual frd number of two persons.
       * (frd1, frd2, +n)->(frd1, frd2) has to be satisfied in order to be 
       * put in to Constraint set.
       * Besides, it must satisfy the probability that the clause be into the Constraint set.
       */
      for (frdPredict <- frdsArr) {
	      val num = findNumMutualFrdsLocal(frdPredict.src1, frdPredict.src2);
	      // if(num>0)
	      //   println(frdPredict.src1 + " " + frdPredict.src2 + " " + num + " "+ probCommonFrd(num) + "  " + computeWeightBasedonNumber(num))

	      if (num > 0 && rand.nextDouble() < computeWeightBasedonNumber(num)) {
	      	if( (probCommonFrd(num) >= 0.5 && frdPredict.result == true) || 
	      			(probCommonFrd(num) < 0.5 && frdPredict.result == false) ){
		        val pred1 = MutualFrd(num, frdPredict.src1, frdPredict.src2);
		        pred1.result = true;
		        val aClause = new Clause(pred1, frdPredict, num);
		        clausesArr.append(aClause);
		        clausesMap += (frdPredict.src1, frdPredict.src2) -> (aClause, null)
	      	}
	      }

      }

      /*
       * After the clausesArr is selected, we then let the arrays run on walkSAT procedure.
       */
      println("before access clausesArr size = " + clausesArr.size);
      walkSAT(clausesArr, clausesMap);
	    frdsMapGlobal = frdsMapLocal

      // val num = findNumMutualFrds(queryFrdPredict.src1, queryFrdPredict.src2)
      // println(num + "  " + queryFrdPredict.result)
      if(frdsMapGlobal(src1).contains(src2) == true ) counter += 1
    }

    println("If the the two people know each other = " + counter.toFloat/conf.getInt("MCSATSampleNum"))
    counter.toFloat/conf.getInt("MCSATSampleNum")
  }

  def walkSAT(clausesArr: ArrayBuffer[Clause], clausesMap: HashMap[(Int, Int), (Clause, Clause)]) = {
  	// assign randome values to all uncertain nodes
    assignBin(clausesArr);

    println("access walkSAT num: " + clausesArr.length)
    /*
     * Use frdsMapLocal to try solving salkSAT problem.
     * For current information in clausesArr, we update the situations in clausesArr.
     * For examples, if (frd1, frd2) is true now in clausesArr, we make it an edge (frd1, frd2)
     * in frdsMapLocal as well.
     */
    frdsMapLocal = frdsMapGlobal.clone();
    for(cl<- clausesArr){
    	val frd1 = cl.pred2.src1;
    	val frd2 = cl.pred2.src2;

    	if(cl.pred2.result == true){
    		if(frdsMapLocal(frd1).contains(frd2) == false){
					frdsMapLocal(frd1) += frd2
          if(frdsMapLocal(frd2) == null) frdsMapLocal(frd2) = ArrayBuffer[Int]()
					frdsMapLocal(frd2) += frd1
    		}
    	}

    	if(cl.pred2.result == false){
    		if(frdsMapLocal(frd1).contains(frd2) == true){
					frdsMapLocal(frd1).remove(frdsMapLocal(frd1).indexOf(frd2))
          if(frdsMapLocal(frd2) != null)
					frdsMapLocal(frd2).remove(frdsMapLocal(frd2).indexOf(frd1))
    		}
    	}

    }


    var wrongClauses = clausesArr.filter { clau => clau.result() == false }
    var wrongClausesNum = wrongClauses.length;

    // if there still has clauses which is false.
    var iterWalkSAT = 0
    while (wrongClausesNum > 0 && iterWalkSAT < conf.getInt("maxWalkSAT")){

      val selectedClause = wrongClauses(rand.nextInt(wrongClauses.length));
      val (frd1, frd2) = (selectedClause.pred2.src1, selectedClause.pred2.src2)
      
			/*
       *---flip FrdPredict
			 * it returns a value that if the whole graph needs to add to an edge, or remove an edge,
			 * or we don't need to do anything.
			 */
      val returnVal = flip(selectedClause.pred2)

      if(returnVal != 0){
	      //adjust other clauses, since one edge is missing or added
		    for(j<-frdsMapLocal(frd1); if j != frd1){
		    	adjustOtherClause(clausesMap, frd2, frd1, j, returnVal);
		    }
		    for(j<-frdsMapLocal(frd2); if j != frd2){
		    	adjustOtherClause(clausesMap, frd1, frd2, j, returnVal);
		    }
    	}

	    wrongClauses = clausesArr.filter { clau => clau.result() == false }
	    wrongClausesNum = wrongClauses.length;
	    if(iterWalkSAT % 2000 == 0)
      	println("iterWalkSAT=" + iterWalkSAT + "  wrongClausesNum = " + wrongClausesNum);
      iterWalkSAT += 1;
    }
  }

  def adjustOtherClause(clausesMap: HashMap[(Int, Int), (Clause, Clause)], frdTarget:Int, frdsrc: Int, j: Int, retVal: Int)={
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
  			cl.setN(cl.n-1)
  		}
   		if(retVal > 0 && frdsMapLocal(j).contains(frdTarget)){
  			cl.setN(cl.n+1)
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
  def flip(frdPredict: FrdPredict) = if(frdPredict.changeEnable){

		val (src1, src2) = (frdPredict.src1, frdPredict.src2)
		if(frdPredict.result == true){
			if(frdsMapLocal(src1).contains(src2)){
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

			frdsMapLocal(src1) += src2
			frdsMapLocal(src2) += src1
			frdPredict.result = true;
			1
		}
	}
	else 0

}
