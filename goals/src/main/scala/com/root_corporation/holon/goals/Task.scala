package com.root_corporation.holon.goals

import scala.concurrent._
import ExecutionContext.Implicits.global

class Task[T](val run: () => Future[T]) {
  def timeSpent: Double = ???
  def intensityOfFocus: Double = ???
}

object Task {
  def apply[T](run: () => Future[T]) = new Task(run)
}
