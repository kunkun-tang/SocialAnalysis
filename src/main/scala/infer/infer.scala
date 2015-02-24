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
  var commsMapGlobal = Map[Int, ArrayBuffer[Int]]()

  def ifTwoPersonKnow(src: Int, dest: Int) = frdsMapGlobal(src).contains(dest)

  def findNumMutualFrds(src1: Int, src2: Int) = {
    val frds1 = frdsMapGlobal(src1);
    val frds2 = frdsMapGlobal(src2);
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
     * In the first time, we need to give random boolean value to all friends pairs.
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
      // if (i1 == src1 && j1 == src2) {
      //   if(ifTwoPersonKnow(i1, j1)) println("They already know each other")
      //   queryFrdPredict = frdsArr(frdsArr.size - 1)
      //   queryFrdPredict.result = false;
      // }
    }

    /*
     * ClausesArr is clauses set, which includes satisfied clauses.
     * clausesMap is clauses HashMap
     * (src1, src2) ->  (muturalFrdClause, mutualCommClause)
     */
    var clausesArr = new ArrayBuffer[Clause]();
    var clausesMap = new HashMap[(Int, Int), (Clause, Clause)]();

    for (frdPredict <- frdsArr; if frdPredict.result == true) {
      val num = findNumMutualFrds(frdPredict.src1, frdPredict.src2);
      // if(num>0)
      //   println(frdPredict.src1 + " " + frdPredict.src2 + " " + num + " "+ probCommonFrd(num) + "  " + computeWeightBasedonNumber(num))
      if (num > 0 && rand.nextDouble() < computeWeightBasedonNumber(num)) {
        val pred1 = MutualFrd(num, frdPredict.src1, frdPredict.src2);
        pred1.result = true;
        val aClause = new Clause(pred1, frdPredict, num);
        clausesArr.append(aClause);
        clausesMap += (frdPredict.src1, frdPredict.src2) -> (aClause, null)
      }
    }

    walkSAT(clausesArr);


    /*
     * The MC-SAT sample loop starts from 1 to a sample upper limit.
     */
    for (sampleNum <- 1 to conf.getInt("MCSATSampleNum")) {

      /*
       * ClausesArr is clauses set, which includes satisfied clauses.
       * ClausesArr is clauses HashMap
       * (src1, src2) ->  (muturalFrdClause, mutualCommClause)
       */
      val clausesArr = new ArrayBuffer[Clause]();
      val clausesMap = new HashMap[(Int, Int), (Clause, Clause)]();

      /**
       * The iteration begins from finding the mutual frd number of two persons.
       * ifTwoPersonKnow(frd.src1, frd.src2) == false has to be satisfied when put in to Constraint set.
       * since it is highly possible that two persons don't know each other.
       *
       * Besides, it must satisfy the probability that the clause be into the Constraint set.
       */
      for (frd <- frdsArr) {
        val num = findNumMutualFrds(frd.src1, frd.src2);

        if (num > 0 && rand.nextDouble() < computeWeightBasedonNumber(num)) {
          val pred1 = MutualFrd(num, frd.src1, frd.src2);
          pred1.result = true;
          val aClause = new Clause(pred1, frd, num);
          clausesArr.append(aClause);
          clausesMap += (frd.src1, frd.src2) -> (aClause, null)
        }
      }

      // for (frd <- frdsArr) {
      //   val num = findNumMutualComms(frd.src1, frd.src2);

      //   if (num > 0 && rand.nextDouble() < computeWeightBasedonNumber(num)) {
      //     val pred1 = MutualComm(num, frd.src1, frd.src2);
      //     pred1.result = true;
      //     val aClause = new Clause(pred1, frd, num);
      //     clausesArr.append(aClause);
      //     if (clausesMap.contains((frd.src1, frd.src2))) {
      //       var v = clausesMap((frd.src1, frd.src2))
      //       clausesMap((frd.src1, frd.src2)) = (v._1, aClause);
      //     } else clausesMap += (frd.src1, frd.src2) -> (null, aClause)
      //   }
      // }

      /*
       * After the clausesArr is selected, we then let the arrays run on walkSAT procedure.
       */
      println("clausesArr size = " + clausesArr.size);
      walkSAT(clausesArr);
      val num = findNumMutualFrds(queryFrdPredict.src1, queryFrdPredict.src2)
      println(num + "  " + queryFrdPredict.result)
    }
  }

  def walkSAT(clausesArr: ArrayBuffer[Clause]) = {
    assignBin(clausesArr);
    var wrongClausesNum = 1;
    while (wrongClausesNum > 0) {

      var wrongClauses = clausesArr.filter { clau => clau.result() == false }
      wrongClauses.foreach { wc =>
        {
          if (wc.result() == false) {
            flip(wc.pred2)
          }
        }
      }
      wrongClausesNum = wrongClauses.size;
      println("wrongClausesNum = " + wrongClausesNum);
    }
  }
}
