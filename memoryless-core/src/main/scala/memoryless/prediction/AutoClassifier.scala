package memoryless.prediction

import org.apache.spark.{SparkConf, SparkContext}
import org.apache.spark.mllib.clustering.KMeans
import org.apache.spark.mllib.linalg.Vectors


/**
  * Created by george.cherian on 2/21/17.
  */





object AutoClassifier {

  val sc = SparkContext.getOrCreate(new SparkConf().setMaster("local[*]").setAppName("Auto Classifier"))


  def classify(dimension:Int,noOfClusters:Int,raw:Seq[Double]):Seq[Data] = {
    val dense = raw.map(Vectors.dense(_))
    val rdd = sc.parallelize(dense)
    var clusters = KMeans.train(rdd,noOfClusters,1000)
    val centers = clusters.clusterCenters
    centers.deep.foreach(println)
    centers.zipWithIndex.map(vi=> {
      new Data(vi._1.toArray.mkString(","),dimension,vi._2)
    })

  }



}
