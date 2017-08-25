package scopt

import OptionDef._
import collection.mutable.{ListBuffer, ListMap}

private[scopt] object Renderer {
  def usage(p: Parser[_, _], mode: RenderingMode): String =
    renderUsage(p, mode)

  def renderUsage(p: Parser[_, _], mode: RenderingMode): String = {
    import p._
    def header: String = (heads map {optionUsage(_, p)}).mkString(NL)

    def renderOneColumnUsage: String =
      {
        val descriptions = optionsForRender map { optionUsage(_, p) }
        (if (header == "") "" else header + NL) +
        usageExample(p) +
        descriptions.mkString(NL)
      }

    def renderTwoColumnsUsage: String =
      {
        val xs = optionsForRender
        val descriptions = {
          val col1Len = math.min(column1MaxLength, xs map { usageColumn1(_, p).length + WW.length} match {
            case Nil => 0
            case list => list.max
          })
          xs map { usageTwoColumn(_, p, col1Len) }
        }
        (if (header == "") "" else header + NL) +
        usageExample(p) +
        descriptions.mkString(NL)
      }

    def optionsForRender: List[ParserDef[_, _]] = {
      val unsorted = p.toList filter { o => o.kind != Head &&
        o.kind != ProgramName &&
        o.kind != Check &&
        !o.isHidden }
      unsorted
    }

    mode match {
      case RenderingMode.OneColumn  => renderOneColumnUsage
      case RenderingMode.TwoColumns => renderTwoColumnsUsage
    }
  }

  def optionUsage(opt: ParserDef[_, _], p: Parser[_, _]): String =
    {
      import opt._
      kind match {
        case Head | Note | Check => _desc
        case Cmd =>
          "Command: " + commandExample(p, Some(opt)) +  NL + _desc
        case Arg => WW + name + NLTB + _desc
        case Opt if read.arity == 2 =>
          WW + (_shortOpt map { o => "-" + o + ":" + keyValueString + " | " } getOrElse { "" }) +
          fullName + ":" + keyValueString + NLTB + _desc
        case Opt if read.arity == 1 =>
          WW + (_shortOpt map { o => "-" + o + " " + valueString + " | " } getOrElse { "" }) +
          fullName + " " + valueString + NLTB + _desc
        case Opt =>
          WW + (_shortOpt map { o => "-" + o + " | " } getOrElse { "" }) +
          fullName + NLTB + _desc
      }
    }

  def usageTwoColumn(opt: ParserDef[_, _], p: Parser[_, _], col1Length: Int): String =
    {
      import opt._
      def spaceToDesc(str: String) = if (str.length <= col1Length) str + " " * (col1Length - str.length)
                                     else str.dropRight(WW.length) + NL + " " * col1Length
      kind match {
        case Head | Note | Check | ProgramName => _desc
        case Cmd => usageColumn1(opt, p) + _desc
        case Arg => spaceToDesc(usageColumn1(opt, p) + WW) + _desc
        case Opt if read.arity == 2 => spaceToDesc(usageColumn1(opt, p) + WW) + _desc
        case Opt if read.arity == 1 => spaceToDesc(usageColumn1(opt, p) + WW) + _desc
        case Opt => spaceToDesc(usageColumn1(opt, p) + WW) + _desc
      }
    }

  def usageColumn1(opt: ParserDef[_, _], p: Parser[_, _]): String =
    {
      import opt._
      kind match {
        case Head | Note | Check | ProgramName => ""
        case Cmd =>
          "Command: " + commandExample(p, Some(opt)) + NL
        case Arg => WW + name
        case Opt if read.arity == 2 =>
          WW + (_shortOpt map { o => "-" + o + ", " } getOrElse { "" }) +
          fullName + ":" + keyValueString
        case Opt if read.arity == 1 =>
          WW + (_shortOpt map { o => "-" + o + ", " } getOrElse { "" }) +
          fullName + " " + valueString
        case Opt =>
          WW + (_shortOpt map { o => "-" + o + ", " } getOrElse { "" }) +
          fullName
      }
    }

  def usageExample(p: Parser[_, _]): String =
    p.programNameOpt match {
      case Some(pn) => "Usage: " + commandExample(p, None) + NLNL
      case _        => ""
    }

  def commandExample(p: Parser[_, _], cmd: Option[ParserDef[_, _]]): String = {
    import p._
    val text = new ListBuffer[String]()
    text += cmd map { commandName(p, _) } getOrElse programName
    val parentId = cmd map {_.id}

    // val cs = commands filter {_.getParentId == parentId}
    // if (cs.nonEmpty) text += cs map {_.name} mkString("[", "|", "]")
    // val os = options.toSeq filter { case x => x.kind == Opt && x.getParentId == parentId }
    // val as = arguments filter {_.getParentId == parentId}
    // if (os.nonEmpty) text += "[options]"
    // if (cs exists { case x => arguments exists {_.getParentId == Some(x.id)}}) text += "<args>..."
    // else if (as.nonEmpty) text ++= as map {_.argName}
    text.mkString(" ")
  }

  def commandName(p: Parser[_, _], cmd: ParserDef[_, _]): String = {
    import p._
    // (cmd.getParentId match {
    //   case Some(x) => (commands find {_.id == x} map {commandName} getOrElse {""}) + " "
    //   case _       => ""
    // }) + cmd.name
    cmd.name
  }

    // {
    //   val headers = p.toList filter { _._kind == Head }
    //   val headerUsage = (headers map { _._desc }).mkString("", NL, NL)
    //   headerUsage
    // }
}
