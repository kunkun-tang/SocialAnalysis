package execute

import com.typesafe.config.ConfigFactory
import preComp._
import infer._
import scala.util.Random
import scala.collection.mutable.ArrayBuffer

object DBLPTruePositiveSelective extends App{

  import preComp.Util._

  val conf = ConfigFactory.load
  val datasetName = conf.getString("DataSetName");
  var (frdsMap, commsMap, backBone) = PreMain.applyDB(datasetName)
  val rand = new Random(System.currentTimeMillis())

  val testData = SelectUtilKnown.readSelectFile("selectKnown_4_4.txt");
  frdsMap = Util.genFrdsMapFromDB(datasetName)

  def getTwoPerson()={
    
    val testDataSize  = testData.size;
    val pair = testData(rand.nextInt(testDataSize));

    (pair._1, pair._2)
  }

  /*
   * frdsPair consist of sample pair of two nodes, which have mutual friend number information.
   * commsPair includes the mutual community number key value.
   */
  // val (frdsPair, commsPair) = Util.sampleUniformQueryNodes(2, frdsMap, commsMap);

  // resultMap collects every source node pair, [(src1, src2), (probability, Baseline, numMutualFrds, numMutualComm)]
  val resultMap = scala.collection.mutable.HashMap[(Int, Int), (Double, Double, Int, Int, Int)]();
  var count =0; var additionProbSAT = 0.0; var additionProbBaseline=0.0; var filterCount = 0;
  for(i <- 1 to conf.getInt("SampleLIMIT")){
    val frdsMapLocal = frdsMap.clone();
    var (src1, src2) = getTwoPerson();

    if(src1 > src2){
      var temp = src1;
      src1 = src2
      src2 = temp;
    }
    val numMutualActualFrds = findNumMutualFrds(src1, src2, frdsMapLocal);
    val numMutualActualComm = findNumMutualComms(src1, src2, commsMap);

    if(numMutualActualFrds >=0){
      val localGraph = PageRankWalk.apply(frdsMapLocal, backBone)(src1)
      val localGraph2 = PageRankWalk.apply(frdsMapLocal, backBone)(src2)
      val fiveSet = localGraph ++ localGraph2  ++ backBone

      /*
       * inferFrdsMap is the final fiveSet frdsMapLocal.
       */
      val inferFrdsMap = Util.prune(frdsMapLocal, fiveSet);
      println("fiveSet size = " + inferFrdsMap.size)

      val (probSAT, probBaseline) = MCSAT(inferFrdsMap, commsMap)(src1, src2)
      if(probSAT > 0.20) filterCount+=1;
      println(" actual num mutual frds = " + numMutualActualFrds);
      println(" actual num mutual Comm = " + numMutualActualComm);
      resultMap += (src1, src2)->(probSAT, probBaseline, numMutualActualFrds, numMutualActualComm, inferFrdsMap.size);
      count += 1;
      additionProbBaseline += (1-probBaseline)*(1-probBaseline);
      additionProbSAT += (1-probSAT)*(1-probSAT);
    }
  }

  println("The ratio with filter eaqual to 0.20  ="+filterCount.toFloat/count)

  printToFile(new java.io.File("result.txt"))(p => {
    resultMap.foreach(res => p.println( "src1 and src2 = " + res._1._1 + "\t"+ res._1._2 + "\t" + " probSAT =" +  res._2._1
    +"\t" + " probBaseline =" +  res._2._2 + "\t"  + " mutual Frd num=" + res._2._3 +  "\t" + " mutual Comm num=" + res._2._4 + " fiveSet size=" + res._2._5))
    p.println("additionProbBaseline sqrt = " + scala.math.sqrt(additionProbBaseline/count) )
    p.println("additionProbSAT sqrt = " + scala.math.sqrt(additionProbSAT/count) )
  
  })

  println(resultMap);
}


object DBLPFalsePositiveSelective extends App{

  import preComp.Util._

  val conf = ConfigFactory.load
  val datasetName = conf.getString("DataSetName");
  var (frdsMap, commsMap, backBone) = PreMain.applyDB(datasetName)
  val rand = new Random(System.currentTimeMillis())

  val testData = SelectUtilKnown.readSelectFile("selectStranger_1_1.txt");
  frdsMap = Util.genFrdsMapFromDB(datasetName)

  def getTwoPerson()={
    
    val testDataSize  = testData.size;
    val pair = testData(rand.nextInt(testDataSize));

    (pair._1, pair._2)
  }

  /*
   * frdsPair consist of sample pair of two nodes, which have mutual friend number information.
   * commsPair includes the mutual community number key value.
   */
  // val (frdsPair, commsPair) = Util.sampleUniformQueryNodes(2, frdsMap, commsMap);

  // resultMap collects every source node pair, [(src1, src2), (probability, Baseline, numMutualFrds, numMutualComm)]
  val resultMap = scala.collection.mutable.HashMap[(Int, Int), (Double, Double, Int, Int, Int)]();
  var count =0; var additionProbSAT = 0.0; var additionProbBaseline=0.0; var filterCount = 0;
  for(i <- 1 to conf.getInt("SampleLIMIT")){
    val frdsMapLocal = frdsMap.clone();
    var (src1, src2) = getTwoPerson();

    if(src1 > src2){
      var temp = src1;
      src1 = src2
      src2 = temp;
    }
    val numMutualActualFrds = findNumMutualFrds(src1, src2, frdsMapLocal);
    val numMutualActualComm = findNumMutualComms(src1, src2, commsMap);

    if(numMutualActualFrds >=0){
      val localGraph = PageRankWalk.apply(frdsMapLocal, backBone)(src1)
      val localGraph2 = PageRankWalk.apply(frdsMapLocal, backBone)(src2)
      val fiveSet = localGraph ++ localGraph2  ++ backBone

      /*
       * inferFrdsMap is the final fiveSet frdsMapLocal.
       */
      val inferFrdsMap = Util.prune(frdsMapLocal, fiveSet);
      println("fiveSet size = " + inferFrdsMap.size)

      val (probSAT, probBaseline) = MCSAT(inferFrdsMap, commsMap)(src1, src2)
      if(probSAT > 0.20) filterCount+=1;
      println(" actual num mutual frds = " + numMutualActualFrds);
      println(" actual num mutual Comm = " + numMutualActualComm);
      resultMap += (src1, src2)->(probSAT, probBaseline, numMutualActualFrds, numMutualActualComm, inferFrdsMap.size);
      count += 1;
      additionProbBaseline += (0-probBaseline)*(0-probBaseline);
      additionProbSAT += (0-probSAT)*(0-probSAT);
    }
  }

  println("The ratio with filter eaqual to 0.20  ="+filterCount.toFloat/count)

  printToFile(new java.io.File("result_3_3_False.txt"))(p => {
    resultMap.foreach(res => p.println( "src1 and src2 = " + res._1._1 + "\t"+ res._1._2 + "\t" + " probSAT =" +  res._2._1
    +"\t" + " probBaseline =" +  res._2._2 + "\t"  + " mutual Frd num=" + res._2._3 +  "\t" + " mutual Comm num=" + res._2._4 + " fiveSet size=" + res._2._5))
    p.println("additionProbBaseline sqrt = " + scala.math.sqrt(additionProbBaseline/count) )
    p.println("additionProbSAT sqrt = " + scala.math.sqrt(additionProbSAT/count) )
  
  })

  println(resultMap);
}