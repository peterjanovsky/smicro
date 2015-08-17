package com.pjanof.benchmarking

/* microbenchmarking
 *
 * measures in nano seconds
 * requires manual JVM warmup through execution of prior run
 *
*/
trait Enum[A] {
  trait Value { self: A => }
  val values: List[A]
}

sealed trait RunType extends RunType.Value
object RunType extends Enum[RunType] {
  case object Parallel extends RunType
  case object Sequential extends RunType
  val values = List(Parallel, Sequential)
}

trait MicroTime {
  def start: Long
  def stop: Long
}

case class MicroResult[T](
  result: T,
  start: Long,
  stop: Long
) extends MicroTime

case class Bucket[T](
  results: IndexedSeq[MicroResult[T]],
  start: Long,
  stop: Long
) extends MicroTime

case class MicroStatistics(
  objName: String,
  runType: RunType,
  repetitions: Int,
  totalTime: Long,
  avgTime: Long,
  tps: Long,
  min: Long,
  max: Long,
  mean: Long,
  median: Long,
  deviation: Double
) {
  def printMicro =
    printf(
      "Name: %s\tRepeat: %d\tTotal: %,d μs \tAvg: %,d μs \tTPS: %d\tMin: %,d μs \tMax: %,d μs \tMean: %,d μs \tMedian: %,d μs \tSTD: %f\n",
      objName,
      repetitions,
      totalTime/1000,
      avgTime/1000,
      tps,
      min/1000,
      max/1000,
      mean/1000,
      median/1000,
      deviation/1000
    )
  def printMillis =
    printf(
      "Name: %s\tRepeat: %d\tTotal: %,d ms\tAvg: %,d ms\tTPS: %d\tMin: %,d ms\tMax: %,d ms\tMean: %,d ms\tMedian: %,d ms\tSTD: %f\n",
      objName,
      repetitions,
      totalTime/1000000,
      avgTime/1000000,
      tps,
      min/1000000,
      max/1000000,
      mean/1000000,
      median/1000000,
      deviation/1000000
    )
}

private object MicroCore {

  def execute[A](x: => A): MicroResult[A] = {
    val start: Long = System.nanoTime
    val res = x
    val stop: Long = System.nanoTime

    MicroResult(res, start, stop)
  }

  def sort[A](x: IndexedSeq[MicroResult[A]]): IndexedSeq[MicroResult[A]] =  x.sortWith( (f, s) => (f.stop - f.start) < (s.stop - s.start) )

  private def diffCalc[A](x: IndexedSeq[MicroResult[A]]): Long = x.foldLeft( 0L ){ (acc, y) => acc + (y.stop - y.start) }
  def min[A](x: IndexedSeq[MicroResult[A]]): Long = diffCalc( sort(x).take(1) )
  def max[A](x: IndexedSeq[MicroResult[A]]): Long = diffCalc( sort(x).takeRight(1) )

  def mean[A](x: IndexedSeq[MicroResult[A]]): Long = diffCalc( x ) / x.length
 
  def median[A](x: IndexedSeq[MicroResult[A]]): Long = {
    val o = sort(x)
    val (l, n) = o.splitAt( o.length / 2 )
    o.length % 2 match {
      case 0 => mean( l.takeRight(1) ++ n.take(1) )
      case 1 => mean( n.take(1) )
    }
  }

  private def square(x: Double): Double = x * x

  import scala.math._

  // square root of the variance (average of squared differences from mean)
  def deviation[A](x: IndexedSeq[MicroResult[A]]): Double = {
    val m: Long = mean(x)
    val v = x.foldLeft( 0D ){ (acc, y) => {
      acc + square( ( y.stop - y.start ) - m )
    } } / x.length
    sqrt(v)
  }
}

object SingleThreadedRunner {

  import scala.math._

  def benchmark[T](reps: Int)(x: => T): Bucket[T] = {

    val start: Long = System.nanoTime
    val result = for( i <- 1 to reps ) yield( MicroCore.execute(x) )
    val stop: Long  = System.nanoTime

    Bucket(result, start, stop)
  }

  def min[T](x: Bucket[T]): Long = MicroCore.min(x.results)
  def max[T](x: Bucket[T]): Long = MicroCore.max(x.results)

  def mean[T](x: Bucket[T]): Long = MicroCore.mean(x.results)
  def median[T](x: Bucket[T]): Long = MicroCore.median(x.results)

  def deviation[T](x: Bucket[T]): Double = MicroCore.deviation(x.results)

  def totalTime[T](x: Bucket[T]): Long = x.stop - x.start
  def avgTime[T](x: Bucket[T]): Long = totalTime(x) / x.results.length

  private def validateAgainstTime(time: Double): Boolean = if ( time > 1.0 ) true else false

  // battle harden in the future => calculation
  def tps[T](x: Bucket[T]): Long = x.results.length match {
    case 1 => 1
    case _ => {
      val t = totalTime(x).toDouble / 1000000000.0
      validateAgainstTime(t) match {
        case true => {
          val res = round( x.results.length.toDouble / t )
          if ( res <= 0 ) x.results.length else res
        }
        case false => x.results.length
      }
    }
  }

  // battle harden in the future => sort called everytime
  def statistics[T](objName: String, runType: RunType, x: Bucket[T]): MicroStatistics = MicroStatistics(
    objName,
    runType,
    x.results.length,
    totalTime(x),
    avgTime(x),
    tps(x),
    min(x),
    max(x),
    mean(x),
    median(x),
    deviation(x)
  )
}

object MultiThreadedRunner {

  import java.util.concurrent.ExecutorService
  import scala.concurrent._
  import duration.Duration

  def benchmark[T](pool: ExecutorService, reps: Int)(x: => T): Bucket[T] = {

    implicit val ec = ExecutionContext.fromExecutorService(pool)

    val start: Long = System.nanoTime

    val listOfFutures = IndexedSeq.fill(reps)( Future( MicroCore.execute(x) ) )
    val futureList = Future.sequence(listOfFutures)
    val result = Await.result(futureList, Duration.Inf)

    val stop: Long  = System.nanoTime

    Bucket(result, start, stop)
  }

  def min[T](x: Bucket[T]): Long = MicroCore.min(x.results)
  def max[T](x: Bucket[T]): Long = MicroCore.max(x.results)

  def mean[T](x: Bucket[T]): Long = MicroCore.mean(x.results)
  def median[T](x: Bucket[T]): Long = MicroCore.median(x.results)

  def deviation[T](x: Bucket[T]): Double = MicroCore.deviation(x.results)

  def totalTime[T](x: Bucket[T]): Long = x.stop - x.start
  def avgTime[T](x: Bucket[T]): Long = totalTime(x) / x.results.length

  // battle harden in the future => calculation
  def tps[T](x: Bucket[T]): Long = x.results.length match {
    case 1 => 1
    case _ => ( x.results.length / avgTime(x) ) * 1000
  }

  // battle harden in the future => sort called everytime
  def statistics[T](objName: String, runType: RunType, x: Bucket[T]): MicroStatistics = MicroStatistics(
    objName,
    runType,
    x.results.length,
    totalTime(x),
    avgTime(x),
    tps(x),
    min(x),
    max(x),
    mean(x),
    median(x),
    deviation(x)
  )
}

