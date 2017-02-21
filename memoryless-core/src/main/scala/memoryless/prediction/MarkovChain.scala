package memoryless.prediction

import keyless.api.Procedure
import keyless.api.hash.HashableFunction
import keyless.index.{NonUniqueIndex, FullUniqueIndex}

import Array._

case class  Data(id:String, dimension:Int, feature:Int)
class MarkovChain(val dimension:Int, val featureVariations:Int,var transition:Array[Array[Array[Double]]],val threshold:Double = 0.95) {

  def this( dimension:Int,  featureVariations:Int) =
  {
    this(dimension,featureVariations,ofDim(dimension, featureVariations,featureVariations),0.95)
  }

  def this( dimension:Int,  featureVariations:Int, threshold:Double) =
  {
    this(dimension,featureVariations,ofDim(dimension, featureVariations,featureVariations),threshold)
  }

  import java.util.function.{Function ⇒ JFunction, Predicate ⇒ JPredicate}


  implicit def toJavaFunction[A, B](f: Function1[A, B]) = new JFunction[A, B] {
    override def apply(a: A): B = f(a)
  }


  implicit def toJavaPredicate[A](f: Function1[A, Boolean]) = new JPredicate[A] {
    override def test(a: A): Boolean = f(a)
  }



  def deepPrint =  {
    println(transition.deep.mkString("\n"))
  }

  def train(history:FullUniqueIndex[Data]) =
  {
    val rewrite:NonUniqueIndex[Data] = new NonUniqueIndex( new HashableFunction((d:Data)=>d.dimension),new HashableFunction((d:Data)=>d.id))
    history.foreach(new Procedure[Data] {
      override def execute(o: scala.Any): Boolean = {
        if (o != null && o.isInstanceOf[Data])
        rewrite.put(o.asInstanceOf[Data])
        return true
      }
    })

    rewrite.forEachGroup( new Procedure[Data] {
      var previousFeature = -1
      var previousDimension = -1

      override def execute(o: scala.Any): Boolean = {

        if (o.isInstanceOf[Data]) {

          val currentFeature = o.asInstanceOf[Data].feature
          val currentDimension = o.asInstanceOf[Data].dimension

          transition(currentDimension)(currentFeature)(currentFeature)=1

        } else if (o.isInstanceOf[FullUniqueIndex[Data]]) {
          o.asInstanceOf[FullUniqueIndex[Data]].foreach(new Procedure[Data] {
            override def execute(e: scala.Any): Boolean = {

              val currentFeature = e.asInstanceOf[Data].feature
              val currentDimension = e.asInstanceOf[Data].dimension
              if (previousFeature >=0)
                transition(currentDimension)(previousFeature)(currentFeature)+=1
              else
                transition(currentDimension)(currentFeature)(currentFeature)=1
               previousFeature=currentFeature;

              return true;
            }
          })

        }
        return true
      }
    })







  }

  def normalize():Array[Array[Array[Double]]] = {
    val normalized = transition.map(d => {
      val rowSum =d.map(_.sum)

      d.map( row => row zip rowSum map {case (r,s) => if (s>0)r/s else 0 }  )

    })
    println(normalized.deep.mkString("\n"))
    normalized

  }


}