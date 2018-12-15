package scopt

import collection.mutable.ListBuffer
import collection.immutable.ListMap
import scala.collection.{ Seq => CSeq }
import scala.collection.immutable.{ Seq => ISeq }

private[scopt] object ScoptEngine {
  def renderHeader[C](options: List[OptionDef[_, C]]): String = {
    def heads: ISeq[OptionDef[_, C]] = options filter {_.kind == Head}
    import OptionDef._
    (heads map {_.usage}).mkString(NL)
  }

  def renderUsage[C](
      programName: String,
      mode: RenderingMode,
      options: List[OptionDef[_, C]]): String = {

    def heads: ISeq[OptionDef[_, C]] = options filter {_.kind == Head}
    def arguments: ISeq[OptionDef[_, C]] = options filter {_.kind == Arg}
    def commands: ISeq[OptionDef[_, C]] = options filter {_.kind == Cmd}
    def header: String = renderHeader[C](options)

    def optionsForRender: List[OptionDef[_, C]] = {
      val unsorted = options filter { o => o.kind != Head && o.kind != Check && !o.isHidden }
      val (remaining, sorted) = unsorted partition {_.hasParent} match {
        case (p, np) => (ListBuffer() ++ p, ListBuffer() ++ np)
      }
      var continue = true
      while (!remaining.isEmpty && continue) {
        continue = false
        for {
          parent <- sorted
        } {
          val childrenOfThisParent = remaining filter {_.getParentId == Some(parent.id)}
          if (childrenOfThisParent.nonEmpty) {
            remaining --= childrenOfThisParent
            sorted.insertAll((sorted indexOf parent) + 1, childrenOfThisParent)
            continue = true
          }
        }
      }
      sorted.toList
    }

    def renderOneColumnUsage: String = {
      import OptionDef._
      val descriptions = optionsForRender map {_.usage}
      (if (header == "") "" else header + NL) +
      "Usage: " + usageExample + NLNL +
      descriptions.mkString(NL)
    }

    def renderTwoColumnsUsage: String = {
      import OptionDef._
      val xs = optionsForRender
      val descriptions = {
        val col1Len = math.min(column1MaxLength, xs map {_.usageColumn1.length + WW.length} match {
          case Nil => 0
          case list => list.max
        })
        xs map {_.usageTwoColumn(col1Len)}
      }
      (if (header == "") "" else header + NL) +
      "Usage: " + usageExample + NLNL +
      descriptions.mkString(NL)
    }

    def usageExample: String = commandExample(programName, None, options)

    mode match {
      case RenderingMode.OneColumn => renderOneColumnUsage
      case RenderingMode.TwoColumns => renderTwoColumnsUsage
    }
  }

  def commandExample[C](
      programName: String,
      cmd: Option[OptionDef[_, C]],
      options: List[OptionDef[_, C]]
      ): String = {

    def arguments: ISeq[OptionDef[_, C]] = options filter {_.kind == Arg}
    def commands: ISeq[OptionDef[_, C]] = options filter {_.kind == Cmd}
    def commandName(cmd: OptionDef[_, C]): String =
      (cmd.getParentId map { x =>
        (commands find {_.id == x} map {commandName} getOrElse {""}) + " "
      } getOrElse {""}) + cmd.name

    val text = new ListBuffer[String]()
    text += cmd map {commandName} getOrElse programName
    val parentId = cmd map {_.id}
    val cs = commands filter { c => c.getParentId == parentId && !c.isHidden }
    if (cs.nonEmpty) text += cs map {_.name} mkString("[", "|", "]")
    val os = options.toSeq filter { case x => x.kind == Opt && x.getParentId == parentId }
    val as = arguments filter {_.getParentId == parentId}
    if (os.nonEmpty) text += "[options]"
    if (cs exists { case x => arguments exists {_.getParentId == Some(x.id)}}) text += "<args>..."
    else if (as.nonEmpty) text ++= as map {_.argName}
    text.mkString(" ")
  }

  /** parses the given `args`.
   */
  def parse[C](
      args: CSeq[String],
      init: C,
      options: List[OptionDef[_, C]],
      configuration: ScoptConfiguration): Option[C] = {
    var i = 0
    import configuration._
    def heads: ISeq[OptionDef[_, C]] = options filter {_.kind == Head}
    def nonArgs: ISeq[OptionDef[_, C]] = options filter { case x => x.kind == Opt || x.kind == Note }
    def arguments: ISeq[OptionDef[_, C]] = options filter {_.kind == Arg}
    def commands: ISeq[OptionDef[_, C]] = options filter {_.kind == Cmd}
    def checks: ISeq[OptionDef[_, C]] = options filter {_.kind == Check}

    val pendingOptions = ListBuffer() ++ (nonArgs filterNot {_.hasParent})
    val pendingArgs = ListBuffer() ++ (arguments filterNot {_.hasParent})
    val pendingCommands = ListBuffer() ++ (commands filterNot {_.hasParent})
    var occurrences: Map[OptionDef[_, C], Int] = ListMap[OptionDef[_, C], Int]().withDefaultValue(0)
    var _config: C = init
    var _error = false

    def pushChildren(opt: OptionDef[_, C]): Unit = {
      // commands are cleared to guarantee that it appears first
      pendingCommands.clear()

      pendingOptions insertAll (0, nonArgs filter { x => x.getParentId == Some(opt.id) &&
        !pendingOptions.contains(x) })
      pendingArgs insertAll (0, arguments filter { x => x.getParentId == Some(opt.id) &&
        !pendingArgs.contains(x) })
      pendingCommands insertAll (0, commands filter { x => x.getParentId == Some(opt.id) &&
        !pendingCommands.contains(x) })
    }
    def handleError(msg: String): Unit = {
      if (errorOnUnknownArgument) {
        _error = true
        reportError(msg)
      }
      else reportWarning(msg)
    }
    def handleArgument(opt: OptionDef[_, C], arg: String): Unit = {
      opt.applyArgument(arg, _config) match {
        case Right(c) =>
          _config = c
          pushChildren(opt)
        case Left(xs) =>
          _error = true
          xs foreach reportError
      }
    }
    def handleOccurrence(opt: OptionDef[_, C], pending: ListBuffer[OptionDef[_, C]]): Unit = {
      occurrences += (opt -> 1)
      if (occurrences(opt) >= opt.getMaxOccurs) {
        pending -= opt
      }
    }
    def findCommand(cmd: String): Option[OptionDef[_, C]] =
      pendingCommands find {_.name == cmd}
    // greedy match
    def handleShortOptions(g0: String): Unit = {
      val gs =  (0 to g0.size - 1).toSeq map { n => g0.substring(0, g0.size - n) }
      gs flatMap { g => pendingOptions map {(g, _)} } find { case (g, opt) =>
        opt.shortOptTokens("-" + g) == 1
      } match {
        case Some(p) =>
          val (g, option) = p
          handleOccurrence(option, pendingOptions)
          handleArgument(option, "")
          if (g0.drop(g.size) != "") {
            handleShortOptions(g0 drop g.size)
          }
        case None => handleError("Unknown option " + "-" + g0)
      }
    }
    def handleChecks(c: C): Unit = {
      Validation.validateValue(checks flatMap {_.checks})(c) match {
        case Right(c) => // do nothing
        case Left(xs) =>
          _error = true
          xs foreach reportError
      }
    }
    while (i < args.length) {
      pendingOptions find {_.tokensToRead(i, args) > 0} match {
        case Some(option) =>
          handleOccurrence(option, pendingOptions)
          option(i, args) match {
            case Right(v) =>          handleArgument(option, v)
            case Left(outOfBounds) => handleError(outOfBounds)
          }
          // move index forward for gobbling
          if (option.tokensToRead(i, args) > 1) {
            i += option.tokensToRead(i, args) - 1
          } // if
        case None =>
          args(i) match {
            case arg if arg startsWith "--" => handleError("Unknown option " + arg)
            case arg if arg startsWith "-"  =>
              if (arg == "-") handleError("Unknown option " + arg)
              else handleShortOptions(arg drop 1)
            case arg if findCommand(arg).isDefined =>
              val cmd = findCommand(arg).get
              handleOccurrence(cmd, pendingCommands)
              handleArgument(cmd, "")
            case arg if pendingArgs.isEmpty => handleError("Unknown argument '" + arg + "'")
            case arg =>
              val first = pendingArgs.head
              handleOccurrence(first, pendingArgs)
              handleArgument(first, arg)
          }
      }
      i += 1
    }

    pendingOptions.filter(_.hasFallback).foreach { opt =>
      val fallback = opt.getFallback
      if (fallback != null) {
        handleOccurrence(opt, pendingOptions)
        handleArgument(opt, fallback.toString)
      }
    }
    (pendingOptions filter { opt => opt.getMinOccurs > occurrences(opt) }) foreach { opt =>
      if (opt.getMinOccurs == 1) reportError("Missing " + opt.shortDescription)
      else reportError(opt.shortDescription.capitalize + " must be given " + opt.getMinOccurs + " times")
      _error = true
    }
    (pendingArgs filter { arg => arg.getMinOccurs > occurrences(arg) }) foreach { arg =>
      if (arg.getMinOccurs == 1) reportError("Missing " + arg.shortDescription)
      else reportError(arg.shortDescription.capitalize + "' must be given " + arg.getMinOccurs + " times")
      _error = true
    }
    handleChecks(_config)
    if (_error) {
      if (showUsageOnError) showUsageAsError()
      else showTryHelp()
      None
    }
    else Some(_config)
  }
}
