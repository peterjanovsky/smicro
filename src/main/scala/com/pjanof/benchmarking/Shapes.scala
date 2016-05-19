package com.pjanof.benchmarking

case class MicroResult[A](result: A, start: Long, stop: Long)

case class Bucket[A](results: IndexedSeq[MicroResult[A]], totalTime: Long)

case class MicroStatistics(
  objName: String,
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
