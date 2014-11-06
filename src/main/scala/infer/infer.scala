package infer 

import com.typesafe.config.ConfigFactory
import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map
import scala.collection.mutable.HashSet
import scala.util.Random
import scala.math
import probabilitymonad.Distribution._

object Infer{

  abstract class Clause(src1: Int, src2: Int){
  	// result is to differentiate the clause is true or not.
  	val result: Boolean = false;
  }

  case class FrdClause(val numMutualFrds: Int, src1: Int, src2: Int) extends Clause(src1, src2)
  case class CommClause(val numMutualComms: Int, src1: Int, src2: Int) extends Clause(src1, src2)


  val rand = new Random(System.currentTimeMillis())
  var frdsMapGlobal = Map[Int, ArrayBuffer[Int]]()
  var FRAlwaysTrue= Map[(Int, Int), Boolean]()
  var FRDynamic = Map[(Int, Int), Boolean]()
  var formulaMap = Map[Clause, Boolean]()

  def ifTwoPersonKnow(src: Int, dest: Int) = frdsMapGlobal(src).contains(dest)

  def genRandomBoolean() = if(rand.nextDouble()<0.5) true else false

  def assignBin(aFRAlwaysTrue: Map[(Int, Int), Boolean]) = {
    for( (i, j)<- FRAlwaysTrue ){
      if(FRDynamic.contains(i))
        aFRAlwaysTrue += i ->true
      else
        aFRAlwaysTrue += i -> genRandomBoolean()
    }
  }

  def ifFormulaTrue(src1: Int, src2: Int, src3: Int) = 
      false


  def apply(frdsMap: Map[Int, ArrayBuffer[Int]], commMap: Map[Int, ArrayBuffer[Int]])(src1: Int, src2: Int) = {
  
    frdsMapGlobal = frdsMap
    val conf = ConfigFactory.load

    /* 
     * In the whole inferSet, if two people know each other, the two is signed
     * to true. If not, a random boolean is assigned to the relationship.
     */
    for( (i1, i2)<- frdsMap; (j1, j2)<- frdsMap; if(i1<j1) ){
      if(ifTwoPersonKnow(i1,j1)){
        FRAlwaysTrue += (i1,j1)->true
        FRDynamic += (i1,j1)->true
      }
      else
        FRDynamic += (i1,j1)->genRandomBoolean
    }

    /*
     * because FRAlwaysTrue should not contain (k,v) (v<=k), we can ignore half matrix.
     */
    println("formulaMap size = " + formulaMap.size)
    // walkSAT(formulaMap)
  }


  def walkSAT(formulas: Map[(Int, Int, Int), Boolean]) = {
    // val walkFRAlwaysTrue = Map[(Int, Int), Boolean]();
    // assignBin(walkFRAlwaysTrue)
    // for((i1, i2)<- walkFRAlwaysTrue; (j1, j2)<-walkFRAlwaysTrue.filter{ case (k,v) =>k._1==i1._2 })
    //   formulas((i1._1, i1._2, j1._2)) = ifFormulaTrue(i1._1, i1._2, j1._2)
    // var falseFormulas = formulas.filter{ case(k,v)=> v==false}.toList;
    // println("falseFormulas size = "+ falseFormulas.length)
    // var count = 1;
    // while(count< 10000){
    //   val formula = falseFormulas(rand.nextInt(falseFormulas.size)) 
    //   count += 1
    // }
  }
}
