package com.root_corporation.holon.goals

import java.time.LocalDateTime
import java.net.URL

class KeyResult(
    name: String,
    description: Option[String] = None,
    _statusArg: => Option[Double] = None,
    _timeToComplete: Option[LocalDateTime] = None,
    val routines: List[Routine[Any]] = List()
) {
  import KeyResult.ActionItem
  var isCommited = false
  var timeToComplete = _timeToComplete
  var urls = List[URL]()
  var statusArg = _statusArg
  var subKrs = List[KeyResult]()
  var prescribedActions = List[ActionItem]()
  def status =
    statusArg match {
      case None      => 1
      case Some(arg) => arg
    }

  def prescribeAction(action: ActionItem) = {
    prescribedActions = action :: prescribedActions
    this
  }

  def addSubs(krs: KeyResult*) = subKrs = subKrs :++ krs

  def complete(_statusArg: Double = 1.0) = {
    statusArg = Some(_statusArg)
    this
  }

  def commited() = {
    // this is now a commitment
    isCommited = true
    this
  }

  override def toString(): String =
    s"\n${description match {
      case Some(description) => description
      case None              => if (!name.isEmpty) name else super.toString
    }}\n{\n${subKrs.map(kr => kr.toString).foldLeft("")((a, b) => a.concat(b))}${if (subKrs.length > 0) "\n"
    else ""}}\n"

  def addUrl(url: URL) = {
    urls = url :: urls
    this
  }

  def addUrl(url: String) = {
    urls = new URL(url) :: urls
    this
  }

  def by(_timeToComplete: String) = {
    timeToComplete = Some(LocalDateTime.parse(_timeToComplete, formatter))
    this
  }

  def ammendBy(_timeToComplete: String) = {
    timeToComplete = Some(LocalDateTime.parse(_timeToComplete, formatter))
    this
  }
}

object KeyResult {
  type ActionItem = String
  def apply(name: String) = new KeyResult(name)
  def apply(name: String, score: => Double) = {
    new KeyResult(name, _statusArg = Some(score))
  }
  def apply(deadline: LocalDateTime, name: String) =
    new KeyResult(name, _timeToComplete = Some(deadline))
  def apply(pred: => Boolean) =
    new KeyResult("", _statusArg = if (pred) Some(1) else Some(0))
  def apply(pred: => Boolean, description: String) =
    new KeyResult(
      "",
      description = Some(description),
      _statusArg = if (pred) Some(1) else Some(0)
    )
  def apply(pred: => Boolean, subKrs: KeyResult*) = {
    val kr = new KeyResult("", _statusArg = if (pred) Some(0) else Some(0))
    kr.addSubs(subKrs: _*)
    kr
  }
  def apply(pred: => Boolean, description: String, subKrs: KeyResult*) = {
    val kr = new KeyResult(
      "",
      description = Some(description),
      _statusArg = if (pred) Some(1) else Some(0)
    )
    kr.addSubs(subKrs: _*)
    kr
  }
  def apply(description: String, subKrs: KeyResult*) = {
    val kr = new KeyResult("", Some(description))
    kr.addSubs(subKrs: _*)
    kr
  }
  def apply(name: String, description: String, subKrs: KeyResult*) = {
    val kr = new KeyResult(name, Some(description))
    kr.addSubs(subKrs: _*)
    kr
  }
  def apply(nameAndDate: Tuple2[String, LocalDateTime], subKrs: KeyResult*) = {
    val (name, timeToComplete) = nameAndDate
    val kr = new KeyResult(name, _timeToComplete = Some(timeToComplete))
    kr.addSubs(subKrs: _*)
    kr
  }
  def apply(pred: Boolean, timeToComplete: LocalDateTime) =
    new KeyResult(
      "",
      _statusArg = if (pred) Some(1) else Some(0),
      _timeToComplete = Some(timeToComplete)
    )
  def apply(pred: => Boolean, routines: List[Routine[Any]]) =
    new KeyResult(
      "",
      _statusArg = if (pred) Some(1) else Some(0),
      routines = routines
    )
  def apply(name: String, description: String, isComplete: Boolean) = {
    new KeyResult(
      name,
      Some(description),
      _statusArg = if (isComplete) Some(-1) else Some(0)
    )
  }
}
