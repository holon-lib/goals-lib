package com.root_corporation.holon.goals

import java.util.{Date, Timer, TimerTask}
import java.time.{LocalDateTime => DateTime, ZoneId}
import java.time.temporal.TemporalAmount
import java.time.Duration

case class Routine[T](
    thingToDo: Task[T],
    timeToDoIt: DateTime,
    reccurance: TemporalAmount
)(implicit timer: Timer)
    extends TimerTask {
  var pastRunTimes: List[DateTime] = List()
  def schedule(timeToDoIt: DateTime = timeToDoIt) = {
    val date =
      Date.from(timeToDoIt.atZone(ZoneId.systemDefault()).toInstant());
    timer.schedule(this, date)
  }
  def run() = {
    val now = DateTime.now
    pastRunTimes = now :: pastRunTimes
    schedule(now.plus(reccurance))
    thingToDo.run()
  }
  def doneConsistentlyFor(duration: Duration): Boolean = true
}
