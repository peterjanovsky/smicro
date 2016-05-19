package com.pjanof.benchmarking

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.Duration
import scala.math._

/* microbenchmarking
 *
 * measures in nano seconds
 * requires manual JVM warmup through execution of prior run
 *
*/
private object MicroCore {

  def execute[A](f: Future[A])(implicit ec: ExecutionContext): MicroResult[A] = {
    val start: Long = System.nanoTime
    val res = Await.result(f, Duration.Inf)
    val stop: Long = System.nanoTime

    MicroResult(res, start, stop)
  }

  def sort[A](xs: IndexedSeq[MicroResult[A]]): IndexedSeq[MicroResult[A]] =
    xs.sortWith( (f, s) => (f.stop - f.start) < (s.stop - s.start) )

  private def diffCalc[A](xs: IndexedSeq[MicroResult[A]]): Long =
    xs.foldLeft( 0L ){ (acc, y) => acc + (y.stop - y.start) }

  def min[A](xs: IndexedSeq[MicroResult[A]]): Long = diffCalc( sort(xs).take(1) )
  def max[A](xs: IndexedSeq[MicroResult[A]]): Long = diffCalc( sort(xs).takeRight(1) )
  def mean[A](xs: IndexedSeq[MicroResult[A]]): Long = diffCalc( xs ) / xs.length
 
  def median[A](xs: IndexedSeq[MicroResult[A]]): Long = {
    val o = sort(xs)
    val (l, n) = o.splitAt( o.length / 2 )
    o.length % 2 match {
      case 0 => mean( l.takeRight(1) ++ n.take(1) )
      case 1 => mean( n.take(1) )
    }
  }

  private def square(x: Double): Double = x * x

  // square root of the variance (average of squared differences from mean)
  def deviation[A](xs: IndexedSeq[MicroResult[A]]): Double = {
    val m: Long = mean(xs)
    val v = xs.foldLeft( 0D ){ (acc, y) => {
      acc + square( ( y.stop - y.start ) - m )
    } } / xs.length
    sqrt(v)
  }
}

object Benchmarker {

  def benchmark[A](reps: Int)(f: Future[A])(implicit ec: ExecutionContext): Bucket[A] = {

    val start: Long = System.nanoTime
    val result = for( i <- 1 to reps ) yield( MicroCore.execute(f) )
    val stop: Long  = System.nanoTime

    Bucket(result, stop - start)
  }

  def min[A](bucket: Bucket[A]): Long = MicroCore.min(bucket.results)

  def max[A](bucket: Bucket[A]): Long = MicroCore.max(bucket.results)

  def mean[A](bucket: Bucket[A]): Long = MicroCore.mean(bucket.results)

  def median[A](bucket: Bucket[A]): Long = MicroCore.median(bucket.results)

  def deviation[A](bucket: Bucket[A]): Double = MicroCore.deviation(bucket.results)

  def totalTime[A](bucket: Bucket[A]): Long = bucket.totalTime

  def avgTime[A](bucket: Bucket[A]): Long = bucket.totalTime / bucket.results.length

  private def validateAgainstTime(time: Double): Boolean = if ( time > 1.0 ) true else false

  // TODO: address calculation
  def tps[A](bucket: Bucket[A]): Long = bucket.results.length match {
    case 1 => 1
    case _ => {
      val t = bucket.totalTime.toDouble / 1000000000.0
      validateAgainstTime(t) match {
        case true => {
          val res = round( bucket.results.length.toDouble / t )
          if ( res <= 0 ) bucket.results.length else res
        }
        case false => bucket.results.length
      }
    }
  }

  // TODO: sort called everytime
  def statistics[A](objName: String, bucket: Bucket[A]): MicroStatistics = MicroStatistics(
    objName,
    bucket.results.length,
    totalTime(bucket),
    avgTime(bucket),
    tps(bucket),
    min(bucket),
    max(bucket),
    mean(bucket),
    median(bucket),
    deviation(bucket)
  )
}
