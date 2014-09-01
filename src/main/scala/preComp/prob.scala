package preComp

import com.typesafe.config.ConfigFactory
import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map
import scala.util.Random
import probabilitymonad._

object Prob{

  import Distribution._

  // case class Trial(haveFairCoin: Boolean, flips: List[Coin])

  // def bayesianCoin(nflips: Int): Distribution[Trial] = {
  //   for {
  //     haveFairCoin <- tf()
  //     c = if (haveFairCoin) coin else biasedCoin(0.9)
  //     flips <- c.repeat(nflips)
  //   } yield Trial(haveFairCoin, flips)
  // }

  // bayesianCoin(5).given(_.flips.forall(_ == H)).pr(_.haveFairCoin)

  def apply()={
    val die = discrete(0 -> 0.1, 1 -> 0.2, 2->0.7)

    println(die.hist)
    println(die.sample(1))
  }
}
