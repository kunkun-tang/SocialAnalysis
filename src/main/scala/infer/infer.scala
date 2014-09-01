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

  def apply(frdsMap: Map[Int, ArrayBuffer[Int]], inferSet: scala.collection.Set[Int])(src1: Int, src2: Int) = {
  
    val conf = ConfigFactory.load
    
    

  }
}
