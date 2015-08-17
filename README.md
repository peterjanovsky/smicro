# Microbenchmarking Abstractions

## Single Threaded Runner

### Use

```scala
import scala.util.Random

val rand = Random

val res = SingleThreadedRunner.benchmark(100) { rand.nextInt }

val stats = SingleThreadedRunner.statistics("Sample Single Threaded Run", RunType.Sequential, res)
```

### Sample Results

```scala
scala> stats.printMicro
Name: Sample Single Threaded Run  Repeat: 100 Total: 273 μs   Avg: 2 μs   TPS: 100  Min: 1 μs   Max: 18 μs  Mean: 2 μs  Median: 1 μs  STD: 2.519045

scala> stats.printMillis
Name: Sample Single Threaded Run  Repeat: 100 Total: 0 ms Avg: 0 ms TPS: 100  Min: 0 ms Max: 0 ms Mean: 0 ms  Median: 0 ms  STD: 0.002519
```

## Multi-Threaded Runner

### Use

```scala
import scala.util.Random

import java.util.concurrent.Executors
import java.util.concurrent.ExecutorService

val rand = Random

val pool: ExecutorService = Executors.newFixedThreadPool(10)

val res = MultiThreadedRunner.benchmark(pool, 100) { rand.nextInt }

pool.shutdown

val stats = MultiThreadedRunner.statistics("Multi-Threaded Run", RunType.Parallel, res)
```

### Sample Results

```scala
scala> stats.printMicro
Name: Multi-Threaded Run  Repeat: 100 Total: 12,458 μs  Avg: 124 μs   TPS: 0  Min: 1 μs   Max: 26 μs  Mean: 1 μs  Median: 1 μs  STD: 2.628556

scala> stats.printMillis
Name: Multi-Threaded Run  Repeat: 100 Total: 12 ms  Avg: 0 ms TPS: 0  Min: 0 ms Max: 0 ms Mean: 0 ms  Median: 0 ms  STD: 0.002629
```
