package preComp

import com.typesafe.config.ConfigFactory
import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import scala.collection.mutable.Map
import scala.collection.mutable.ArrayBuffer

object Util {

  val rand = new Random(System.currentTimeMillis());
  val SEARCH_MAX_LOOP = 1000;
  def genTwoSourceNodes(frdsMap: Map[Int, ArrayBuffer[Int]]) = {
    var src1 = 0;
    var src2 = 0;
    val frdsMapSize  = frdsMap.size;
    var count = 0;
    while(count < SEARCH_MAX_LOOP && !frdsMap.contains(src1) ){
      src1 = rand.nextInt(frdsMapSize);
      count += 1
    }
    if(count == SEARCH_MAX_LOOP) {
      println("it can not find a good src1.")
      System.exit(0);
    }
    count = 0;
    while(count < SEARCH_MAX_LOOP && !frdsMap.contains(src2) && src1 != src2 ){
      src2 = rand.nextInt(frdsMapSize);
      count+=1;
    }
    if(count == SEARCH_MAX_LOOP) {
      println("it can not find a good src2.")
      System.exit(0);
    }

    (src1, src2)
  }
}
