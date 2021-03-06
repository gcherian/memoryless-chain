package memoryless.prediction

import java.util.UUID

import keyless.index.FullUniqueIndex
import org.scalatest.FunSuite



/**
  * Created by gcherian on 2/7/2017.
  */
class MemorylessSuite extends FunSuite {



  import java.util.function.{Function ⇒ JFunction, Predicate ⇒ JPredicate}


  implicit def toJavaFunction[A, B](f: Function1[A, B]) = new JFunction[A, B] {
    override def apply(a: A): B = f(a)
  }


  implicit def toJavaPredicate[A](f: Function1[A, Boolean]) = new JPredicate[A] {
    override def test(a: A): Boolean = f(a)
  }
  test("classify") {

    val c1= Array[Double](1.2,1.3,1.7,1.8,2.0)
    val c2 = Array[Double](2.1,2.2,2.3,2.6,2.7)

    val d1=AutoClassifier.classify(0,2,c1)
    val d2=AutoClassifier.classify(1,2,c2)
    d1.foreach(println(_))
    d2.foreach(println(_))

  }


  test("train") {
    val chain:MarkovChain = new MarkovChain(3,2)
    val fui:FullUniqueIndex[Data] = new FullUniqueIndex[Data]((d:Data)=>d.id )

    val d1 = new Data(UUID.randomUUID().toString,0,1)
    val d2 = new Data(UUID.randomUUID().toString,0,0)
    val d3 = new Data(UUID.randomUUID().toString,1,0)
    val d4 = new Data(UUID.randomUUID().toString,1,1)
    val d5 = new Data(UUID.randomUUID().toString,0,0)
    val d6 = new Data(UUID.randomUUID().toString,1,0)
    val d7 = new Data(UUID.randomUUID().toString,2,0)
    fui.put(d1) ; fui.put(d2);
    fui.put(d3);fui.put(d4)
    fui.put(d5); fui.put(d6);fui.put(d7)
    chain.train(fui)
    println("Markov Chain.")
    chain.deepPrint
    println("Normalized Chain.")
    chain.normalize()
  }

}
