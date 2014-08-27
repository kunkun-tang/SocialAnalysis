package preComp

import com.typesafe.config.ConfigFactory
import scala.io.Source
import scala.collection.mutable.ArrayBuffer

object PreMain {

  val frdsMap = scala.collection.mutable.Map[Int, ArrayBuffer[Int]]()

  def putFrds(tuple: Array[String])= if(tuple.length == 2){
    val frd1 = tuple(0).toInt
    val frd2 = tuple(1).toInt
    if(frdsMap(frd1) == null) frdsMap(frd1) = ArrayBuffer[Int]()
    if(frdsMap(frd2) == null) frdsMap(frd2) = ArrayBuffer[Int]()
    frdsMap(frd1) += frd2
    frdsMap(frd2) += frd1
  }

  def pruneFrds(aMap: scala.collection.mutable.Map[Int, ArrayBuffer[Int]], smallDegree: Int)=
    for( (k,v)<-aMap; if(v==null || v.size < smallDegree)) aMap -= k

  def apply(dataSetName: String) = {
    val conf = ConfigFactory.load

    /*
     * init the frdsMap by assigning every Int with a null Friends List
     */
    for(i<- 0 to conf.getInt(dataSetName+".maxIDs"))
      frdsMap(i) = null
   
    Source.fromFile(conf.getString(dataSetName+".friendsFile")).getLines.
    foreach{ 
      line => if(line.startsWith("#", 0) == false)
                putFrds(line.split('\t'))
    }

    /*
     * prune people whose friends is null
     */
    pruneFrds(frdsMap, conf.getInt(dataSetName+".filterSmallDegree"))
    println("after prune frdsMap size = " + frdsMap.size)

    /*
     * compute backBoneGraph with configuration.
     */
    val backBone= frdsMap.filter{ case (k,v)=> v.size>conf.getInt(dataSetName+".BackBoneDegree") }
                    .keySet.toList
    println(backBone.length)
    (frdsMap, backBone)
  }
}
