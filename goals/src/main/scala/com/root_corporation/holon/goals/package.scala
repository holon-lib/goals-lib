package com.root_corporation.holon

import scala.concurrent._
import ExecutionContext.Implicits.global
import java.time.{Duration}
import java.util.Timer
import java.util.concurrent.CountDownLatch
import scala.util.Try
import java.util.concurrent.TimeUnit
import java.util.concurrent.TimeoutException
import java.time.format.DateTimeFormatter

package object goals {
  implicit val timer = new Timer

  def timeBound(time: Duration)(f: () => Boolean): Future[Boolean] = {
    val aref = new java.util.concurrent.atomic.AtomicReference[Thread]()

    import ExecutionContext.Implicits.global
    val latch = new CountDownLatch(1)
    Future {
      latch.await() // wait for 2nd future to start
      Thread.sleep(time.toMillis)
      aref.get().interrupt
    }

    Future {
      aref.set(Thread.currentThread)
      try {
        latch.countDown()
        f() //blocks. may return or thread may be inturrupted
      } catch {
        case _: InterruptedException => throw new TimeoutException()
      }
    }
  }

  val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm")
}
