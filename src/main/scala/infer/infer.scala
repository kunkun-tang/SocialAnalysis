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

  val rand = new Random(System.currentTimeMillis())
  var frdsMapGlobal = Map[Int, ArrayBuffer[Int]]()
  var frdsRelation= Map[(Int, Int), Boolean]()
  var frdsRelationEvidence = Map[(Int, Int), Boolean]()
  var formulaMap = Map[(Int, Int, Int), Boolean]()

  def ifTwoPersonKnow(src: Int, dest: Int) = frdsMapGlobal(src).contains(dest)

  def genRandomBoolean() = if(rand.nextDouble()<0.5) true else false

  def assignBin(aFrdsRelation: Map[(Int, Int), Boolean]) = {
    for( (i, j)<- frdsRelation ){
      if(frdsRelationEvidence.contains(i))
        aFrdsRelation += i ->true
      else
        aFrdsRelation += i -> genRandomBoolean()
    }
  }

  def ifFormulaTrue(src1: Int, src2: Int, src3: Int) = 
    if(ifTwoPersonKnow(src1, src2) && ifTwoPersonKnow(src2, src3) && !ifTwoPersonKnow(src1, src3) )
      false
    else true

  def apply(frdsMap: Map[Int, ArrayBuffer[Int]], inferSet: scala.collection.Set[Int])(src1: Int, src2: Int) = {
  
    frdsMapGlobal = frdsMap
    val conf = ConfigFactory.load

    /* 
     * In the whole inferSet, if two people know each other, the two is signed
     * to true. If not, a random boolean is assigned to the relationship.
     */
    for(i<- inferSet; j<- inferSet; if(i<j && !frdsRelation.contains( (i,j) ))){
      if(ifTwoPersonKnow(i,j)){
        frdsRelation += (i,j)->true
        frdsRelationEvidence  += (i,j)->true
      }
      else
        frdsRelation += (i,j)->genRandomBoolean
    }

    /*
     * frdsRelation should not contain (k,v) (v<=k)
     */
    for((i1, i2)<- frdsRelation; (j1, j2)<-frdsRelation.filter{ case (k,v) =>k._1==i1._2}; if(!( i2== true && j2==true && ifTwoPersonKnow(i1._1, j1._2)==false )) ){
      if(rand.nextDouble()<0.3){
        if(ifTwoPersonKnow(i1._1, i1._2) && ifTwoPersonKnow(i1._2, j1._1) )
          println(ifTwoPersonKnow(i1._1, i1._2) + " " + ifTwoPersonKnow(i1._2, j1._1));
        formulaMap += (i1._1, i1._2, j1._2) -> ifFormulaTrue(i1._1, i1._2, j1._2);
      }
    }
    println("formulaMap size = " + formulaMap.size)
    walkSAT(formulaMap)
  }


  def walkSAT(formulas: Map[(Int, Int, Int), Boolean]) = {
    val walkFrdsRelation = Map[(Int, Int), Boolean]();
    assignBin(walkFrdsRelation)
    for((i1, i2)<- walkFrdsRelation; (j1, j2)<-walkFrdsRelation.filter{ case (k,v) =>k._1==i1._2 })
      formulas((i1._1, i1._2, j1._2)) = ifFormulaTrue(i1._1, i1._2, j1._2)
    var falseFormulas = formulas.filter{ case(k,v)=> v==false}.toList;
    println("falseFormulas size = "+ falseFormulas.length)
    var count = 1;
    while(count< 10000){
      val formula = falseFormulas(rand.nextInt(falseFormulas.size)) 
      count += 1
    }
  }
}
