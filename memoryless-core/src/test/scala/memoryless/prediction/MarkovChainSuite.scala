package memoryless.prediction

import keyless.index.FullUniqueIndex
import org.scalatest.FunSuite



/**
  * Created by gcherian on 2/7/2017.
  */
class MarkovChainSuite extends FunSuite {



  import java.util.function.{Function ⇒ JFunction, Predicate ⇒ JPredicate}


  implicit def toJavaFunction[A, B](f: Function1[A, B]) = new JFunction[A, B] {
    override def apply(a: A): B = f(a)
  }


  implicit def toJavaPredicate[A](f: Function1[A, Boolean]) = new JPredicate[A] {
    override def test(a: A): Boolean = f(a)
  }


  test("train") {
    val chain:MarkovChain = new MarkovChain(3,4)(0.95)
    val fui:FullUniqueIndex[Data] = new FullUniqueIndex[Data]((d:Data)=>d.dimension + ":" + d.feature)

    val d1 = new Data(0,0)
    val d2 = new Data(0,1)
    val d3 = new Data(1,0)
    val d4 = new Data(1,1)
    val d5 = new Data(0,0)
    fui.put(d1) ; fui.put(d2);fui.put(d3);fui.put(d4)
    chain.train(fui)
    chain.deepPrint
  }

}
