package memoryless.prediction

import Array._
class MarkovChain(val dimension:Int, val featureLength:Int)(val threshold:Double = 0.95) {

  val transition :Array[Array[Array[Double]]] = ofDim(dimension,featureLength,featureLength)


}