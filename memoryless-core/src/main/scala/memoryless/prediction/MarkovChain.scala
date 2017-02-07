package memoryless.prediction

import keyless.api.Procedure
import keyless.api.hash.HashableFunction
import keyless.index.{NonUniqueIndex, FullUniqueIndex}

import Array._
case class Data(dimension:Int,feature:Int)
class MarkovChain(val dimension:Int, val featureVariations:Int)(val threshold:Double = 0.95) {



  import java.util.function.{Function ⇒ JFunction, Predicate ⇒ JPredicate}


  implicit def toJavaFunction[A, B](f: Function1[A, B]) = new JFunction[A, B] {
    override def apply(a: A): B = f(a)
  }


  implicit def toJavaPredicate[A](f: Function1[A, Boolean]) = new JPredicate[A] {
    override def test(a: A): Boolean = f(a)
  }


  val transition :Array[Array[Array[Double]]] = ofDim(dimension,featureVariations,featureVariations)

  def train(history:FullUniqueIndex[Data]) =
  {
    val rewrite:NonUniqueIndex[Data] = new NonUniqueIndex( new HashableFunction((d:Data)=>d.dimension),history.getStrategy)
    history.foreach(new Procedure[Data] {
      override def execute(o: scala.Any): Boolean = {
        if (o != null && o.isInstanceOf[Data])
        rewrite.put(o.asInstanceOf[Data])
        return true
      }
    })
    rewrite.foreach( new Procedure[Data] {
      var previousFeature = -1
      var previousDimension =0

      override def execute(o: scala.Any): Boolean = {

        if (o.isInstanceOf[Data]) {

          var currentFeature = o.asInstanceOf[Data].feature
          var currentDimension = o.asInstanceOf[Data].dimension
          transition(dimension)(currentFeature)(currentFeature)=1
        } else if (o.isInstanceOf[FullUniqueIndex[Data]]) {
          val fui:FullUniqueIndex[Data] = o.asInstanceOf[FullUniqueIndex[Data]]
          fui.foreach(new Procedure[Data] {
            override def execute(o: scala.Any): Boolean = {
              if (o.isInstanceOf[Data]) {
                val data = o.asInstanceOf[Data]
                if (previousFeature>=0) {
                  transition(data.dimension)(previousFeature)(data.feature)+=1
                }
                previousFeature = data.feature
              }
              return true
            }
          })


        }
        return true
      }
    })

  }


}