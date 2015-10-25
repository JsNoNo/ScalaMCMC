package ScalaMCMC

import java.io.{FileOutputStream, OutputStreamWriter, BufferedWriter}
import java.util.concurrent.{ConcurrentHashMap, ConcurrentLinkedQueue}
import scala.collection.JavaConversions.{mapAsJavaMap, asScalaIterator}
import breeze.linalg.{max, randomInt, DenseMatrix, DenseVector}
import breeze.stats.distributions.{Rand, Gaussian}

object MCMC extends App with Sampling {
  //setup
  val nMC = 100000
  val nThreads = 8
  val seed = 1

  //variables
  sealed trait anyVar
  case object theta extends anyVar
  type V = anyVar
  type S = DenseVector[Double]

  //initial state
  val dim = 8
  val J = DenseMatrix.ones[Double](dim,dim) + (DenseMatrix.eye[Double](dim) :* 0.01)
  val h = DenseVector.zeros[Double](dim)
  val Sigma = invertSymmetricMatrix(J)
  val mu = Sigma * h
  val initial:Map[V,S] = Map(theta -> DenseVector.zeros[Double](dim))


  //latest state and output queue
  val latest = new ConcurrentHashMap[V,S](mapAsJavaMap(initial))
  val out = new ConcurrentLinkedQueue[Map[V,S]]
  out.add(initial)

  //run MCMC
  for(i <- (1 until nMC).par){
    val next = applyKernel(latest)
    latest.put(theta, next.get(theta).orNull)
    out.add(next)

    if (i % max(nMC / 10, 1) == 0) {
      println(s"total samples: $i")
    }
  }

  //print output
  val fileName = "output/out"
  println(s"writing output of length ${out.size}")
  val writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(fileName)))
  for(v <- asScalaIterator(out.iterator)) {
    writer.write(s"${v.get(theta).orNull.data.mkString(",")}\n")
  }

  //internal MCMC functions
  def applyKernel(latest: ConcurrentHashMap[V,S]): Map[V,S] = {
    val oldx = latest.get(theta)
    val nextIdx = Rand.generator.nextInt(oldx.length)

    val next = updateNormal(nextIdx, oldx, mu, Sigma)

    val newx = new DenseVector[Double](oldx.data.updated(nextIdx, next))
    Map(theta -> newx)
  }

  def updateNormal(idx: Int, oldx: DenseVector[Double],
                   mu: DenseVector[Double], Sigma: DenseMatrix[Double]): Double = {
    val sidx = Seq(idx) //need to cast idx to seq to avoid type confusion
    val midx = (0 until oldx.length).filter({ value => value != idx })

    val x2 = oldx(midx)

    val mu1 = mu(sidx)
    val mu2 = mu(midx)

    val Sigma11 = Sigma(sidx,sidx)
    val Sigma22 = Sigma(midx, midx)
    val Sigma12 = Sigma(sidx, midx)
    val Sigma21 = Sigma(midx, sidx)

    val Sigma22inv = breeze.linalg.inv(Sigma22.toDenseMatrix)

    val mustar = (mu1 + (Sigma12 * Sigma22inv * (x2 - mu2))).toArray.head
    val Sigmastar = (Sigma11 - (Sigma12 * Sigma22inv * Sigma21)).toDenseMatrix.toArray.head

    val stddev = breeze.numerics.sqrt(Sigmastar)

    val out = Gaussian(mustar, stddev).draw()
    out
  }

}