package preComp

import com.typesafe.config.ConfigFactory
import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map
import scala.util.Random

object RWM{

  val rand = new Random(System.currentTimeMillis())
  /*
   * The second argument means the source ID initializing random walk.
   */
  def apply(frdsMap: Map[Int, ArrayBuffer[Int]], backBone: List[Int])(source: Int) = {
  
    //val random_index = rand.nextInt(frdsMap(source))
  }
}
