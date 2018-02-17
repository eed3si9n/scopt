package scopt

import OptionDef._
import collection.mutable.{ListBuffer, ListMap}

/**
 * `Builder[C]` is used to capture the configuration class `C`.
 */
sealed abstract class Builder[C] {

  def programName(x: String): Parser[Unit, C] =
    wrap(makeDef[Unit](ProgramName, "")).text(x)

  /** adds usage text. */
  def head(xs: String*): Parser[Unit, C] =
    wrap(makeDef[Unit](Head, "")).text(xs.mkString(" "))

  /** adds an option invoked by `--name x`.
   * @param name name of the option
   */
  def opt[A: Read](name: String): Parser[A, C] =
    wrap(makeDef[A](Opt, name))

  /** adds an option invoked by `-x value` or `--name value`.
   * @param x name of the short option
   * @param name name of the option
   */
  def opt[A: Read](x: Char, name: String): Parser[A, C] =
    opt[A](name).abbr(x.toString)

  /** adds an argument invoked by an option without `-` or `--`.
   * @param name name in the usage text
   */
  def arg[A: Read](name: String): Parser[A, C] =
    wrap(makeDef(Arg, name)).required()

  /** adds a command invoked by an option without `-` or `--`.
   * @param name name of the command
   */
  def cmd(name: String): Parser[Unit, C] =
    wrap(makeDef[Unit](Cmd, name))

  /** adds final check. */
  def checkConfig(f: C => Either[String, Unit]): Parser[Unit, C] =
    wrap(makeDef[Unit](Check, "").validateConfig(f))

  /** call this to express success in custom validation. */
  def success: Either[String, Unit] = OptionDef.makeSuccess[String]

  /** call this to express failure in custom validation. */
  def failure(msg: String): Either[String, Unit] = Left(msg)

  protected def wrap[A](d: ParserDef[A, C]): Parser[A, C] =
    Parser(d, Nil)

  protected def makeDef[A: Read](kind: OptionDefKind, name: String): ParserDef[A, C] =
    ParserDef[A, C](_kind = kind, _name = name)
}

object Builder {
  def apply[C]: Builder[C] = new Builder[C] {}
}

case class Parser[A, C](head: ParserDef[A, C], rest: List[ParserDef[_, C]]) {
  val config = ParseConfig()

  /** Adds description in the usage text. */
  def text(x: String): Parser[A, C] = subHead[A](head.text(x))

  /** Adds short option -x. */
  def abbr(x: String): Parser[A, C] = subHead[A](head.abbr(x))

  /** Adds a callback function. */
  def action(f: (A, C) => C): Parser[A, C] = subHead[A](head.action(f))

  /** Requires the option to appear at least `n` times. */
  def minOccurs(n: Int): Parser[A, C] = subHead[A](head.minOccurs(n))

  /** Allows the argument to appear at most `n` times. */
  def maxOccurs(n: Int): Parser[A, C] = subHead[A](head.maxOccurs(n))

  /** Requires the option to appear at least once. */
  def required(): Parser[A, C] = minOccurs(1)

  /** Chanages the option to be optional. */
  def optional(): Parser[A, C] = minOccurs(0)

  /** Allows the argument to appear multiple times. */
  def unbounded(): Parser[A, C] = maxOccurs(UNBOUNDED)

  /** Hides the option in any usage text. */
  def hidden(): Parser[A, C] = subHead[A](head.hidden())

  /** Adds a parser under this command. */
  def children(c: Parser[A, C]): Parser[A, C] = 
    subHead[A](head.children(c))

  /** Adds custom validation. */
  def validate(f: A => Either[String, Unit]): Parser[A, C] = subHead[A](head.validate(f))

  def toList: List[ParserDef[_, C]] = head :: rest
  def ++(other: Parser[_, C]): Parser[A, C] =
    Parser(head, rest ::: other.toList)

  def foreach(f: Unit => Unit): Unit = f(())

  def map(f: Unit => Unit): Parser[A, C] = this

  def flatMap(f: Unit => Parser[_, C]): Parser[A, C] =
    Parser(head, rest ::: f(()).toList)

  protected def subHead[B](head: ParserDef[B, C]): Parser[B, C] =
    Parser(head, rest)

  private[scopt] lazy val programNameOpt: Option[ParserDef[_, C]] =
    toList find {_.kind == ProgramName}

  private[scopt] lazy val programName: String =
    programNameOpt match {
      case Some(d) => d._desc
      case _       => ""
    }

  private[scopt] lazy val heads: List[ParserDef[_, C]] = toList filter {_.kind == Head}

  def usage: String = Renderer.usage(this, config.renderingMode)
  override def toString: String = s"Parser()"

  def parse(args: Seq[String], init: C): Option[C] =
    ParseEngine.parse(this, args, init, config)
}

case class ParserDef[A: Read, C] private[scopt] (
  _id: Int,
  _kind: OptionDefKind,
  _name: String,
  _shortOpt: Option[String],
  _keyName: Option[String],
  _valueName: Option[String],
  _desc: String,
  _action: (A, C) => C,
  _configValidations: List[C => Either[String, Unit]],
  _validations: List[A => Either[String, Unit]],
  _parentId: Option[Int],
  _parentNames: List[String],
  _minOccurs: Int,
  _maxOccurs: Int,
  _isHidden: Boolean,
  _children: List[ParserDef[_, C]]
) {
  private[scopt] def read: Read[A] = implicitly[Read[A]]


  /** Adds description in the usage text. */
  def text(x: String): ParserDef[A, C] = copy(_desc = x)

  /** Adds short option -x. */
  def abbr(x: String): ParserDef[A, C] = copy(_shortOpt = Some(x))

  /** Requires the option to appear at least `n` times. */
  def minOccurs(n: Int): ParserDef[A, C] = copy(_minOccurs = n)

  /** Allows the argument to appear at most `n` times. */
  def maxOccurs(n: Int): ParserDef[A, C] = copy(_maxOccurs = n)

  /** Hides the option in any usage text. */
  def hidden(): ParserDef[A, C] = copy(_isHidden = true)

  /** Adds opt/arg under this command. */
  def children(x: Parser[_, C]): ParserDef[A, C] =
    copy(_children = x.toList map { child => child.setParent(this) })

  private[scopt] def setParent(parent: ParserDef[_, _]): ParserDef[A, C] =
    copy(_parentId = Option(parent._id), _parentNames = _parentNames :+ parent._name)

  /** Adds custom validation. */
  def validate(f: A => Either[String, Unit]): ParserDef[A, C] =
    copy(_validations = _validations :+ f)

  private[scopt] def validateConfig(f: C => Either[String, Unit]): ParserDef[A, C] =
    copy(_configValidations = _configValidations :+ f)

  /** Adds a callback function. */
  def action(f: (A, C) => C): ParserDef[A, C] =
    copy(_action = (a: A, c: C) => { f(a, _action(a, c)) })

  def parseX(in: String): ParseResult[A] = ???

  private[scopt] val kind: OptionDefKind = _kind
  private[scopt] val id: Int = _id
  private[scopt] val name: String = _name
  private[scopt] def callback: (A, C) => C = _action
  private[scopt] def getMinOccurs: Int = _minOccurs
  private[scopt] def getMaxOccurs: Int = _maxOccurs
  private[scopt] def shortOptOrBlank: String = _shortOpt getOrElse("")
  private[scopt] def hasParent: Boolean = _parentId.isDefined
  private[scopt] def getParentId: Option[Int] = _parentId
  private[scopt] def isHidden: Boolean = _isHidden
  private[scopt] def checks: Seq[C => Either[String, Unit]] = _configValidations


  private[scopt] def applyArgument(arg: String, config: C): Either[Seq[String], C] =
    ParseEngine.applyArgument[A, C](this, arg, config)

  // number of tokens to read: 0 for no match, 2 for "--foo 1", 1 for "--foo:1"
  private[scopt] def shortOptTokens(arg: String): Int =
    _shortOpt match {
      case Some(c) if arg == "-" + shortOptOrBlank                 => 1 + read.tokensToRead
      case Some(c) if arg startsWith ("-" + shortOptOrBlank + ":") => 1
      case Some(c) if arg startsWith ("-" + shortOptOrBlank + "=") => 1
      case _ => 0
    }

  private[scopt] def longOptTokens(arg: String): Int =
    if (arg == fullName) 1 + read.tokensToRead
    else if ((arg startsWith (fullName + ":")) || (arg startsWith (fullName + "="))) 1
    else 0

  private[scopt] def tokensToRead(i: Int, args: Seq[String]): Int =
    if (i >= args.length || kind != Opt) 0
    else args(i) match {
      case arg if longOptTokens(arg) > 0  => longOptTokens(arg)
      case arg if shortOptTokens(arg) > 0 => shortOptTokens(arg)
      case _ => 0
    }

  private[scopt] def token(i: Int, args: Seq[String]): Option[String] =
    if (i >= args.length || kind != Opt) None
    else Some(args(i))

  private[scopt] def keyValueString: String =
    (_keyName getOrElse defaultKeyName) + "=" + valueString

  private[scopt] def valueString: String = (_valueName getOrElse defaultValueName)

  def shortDescription: String =
    kind match {
      case Opt => "option " + fullName
      case Cmd => "command " + fullName
      case _   => "argument " + fullName
    }

  def fullName: String =
    kind match {
      case Opt => "--" + name
      case _   => name
    }
}

object ParserDef {
  def apply[A: Read, C](_kind: OptionDefKind, _name: String): ParserDef[A, C] =
    new ParserDef(_id = generateId,
      _kind = _kind,
      _name = _name,
      _shortOpt = None,
      _keyName = None,
      _valueName = None,
      _desc = "",
      _action = { (a: A, c: C) => c },
      _configValidations = List(),
      _validations = List(),
      _parentId = None,
      _parentNames = Nil,
      _minOccurs = 0,
      _maxOccurs = 1,
      _isHidden = false,
      _children = Nil)
}

sealed abstract class ParseResult[A]

private[scopt] object ParseEngine {
  import platform._

  /** parses the given `args`.
   */
  def parse[C](parser: Parser[_, C], args: Seq[String],
    init: C, config: ParseConfig): Option[C] = {
    val options: List[ParserDef[_, C]] = parser.toList
    var i = 0
    val nonArgs: List[ParserDef[_, C]] = options filter { case x => x.kind == Opt || x.kind == Note }
    val helpOptions: List[ParserDef[_, C]] = options filter { _.name == "help" }
    val arguments: List[ParserDef[_, C]] = options filter {_.kind == Arg}
    val commands: List[ParserDef[_, C]] = options filter {_.kind == Cmd}
    val checks: List[ParserDef[_, C]] = options filter {_.kind == Check}
    val pendingOptions = ListBuffer() ++ (nonArgs filterNot {_.hasParent})
    val pendingArgs = ListBuffer() ++ (arguments filterNot {_.hasParent})
    val pendingCommands = ListBuffer() ++ (commands filterNot {_.hasParent})
    val occurrences = ListMap[ParserDef[_, C], Int]().withDefaultValue(0)
    var _config: C = init
    var _error = false
    import config._

    def pushChildren(opt: ParserDef[_, C]): Unit = {
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
    def handleArgument(opt: ParserDef[_, C], arg: String): Unit = {
      opt.applyArgument(arg, _config) match {
        case Right(c) =>
          _config = c
          pushChildren(opt)
        case Left(xs) =>
          _error = true
          xs foreach reportError
      }
    }
    def handleOccurrence(opt: ParserDef[_, C], pending: ListBuffer[ParserDef[_, C]]): Unit = {
      occurrences(opt) += 1
      if (occurrences(opt) >= opt.getMaxOccurs) {
        pending -= opt
      }
    }
    def findCommand(cmd: String): Option[ParserDef[_, C]] =
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
          applyOption(option, i, args) match {
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
      if (showUsageOnError) showUsageAsError(parser.usage)
      else showTryHelp(helpOptions)
      None
    }
    else Some(_config)
  }

  private[scopt] def applyOption[C](opt: ParserDef[_, C], i: Int, args: Seq[String]): Either[String, String] =
    if (i >= args.length || opt.kind != Opt) Left("Option does not match")
    else args(i) match {
      case arg if opt.longOptTokens(arg) == 2 || opt.shortOptTokens(arg) == 2 =>
        opt.token(i + 1, args) map {Right(_)} getOrElse Left("Missing value after " + arg)
      case arg if opt.longOptTokens(arg) == 1 && opt.read.tokensToRead == 1 =>
        Right(arg drop (opt.fullName + ":").length)
      case arg if opt.shortOptTokens(arg) == 1 && opt.read.tokensToRead == 1 =>
        Right(arg drop ("-" + opt.shortOptOrBlank + ":").length)
      case _ => Right("")
    }

  def applyArgument[A: Read, C](opt: ParserDef[A, C], arg: String, config: C): Either[Seq[String], C] =
    try {
      val x = opt.read.reads(arg)
      Validation.validateValue(opt._validations)(x) match {
        case Right(_) => Right(opt.callback(x, config))
        case Left(xs) => Left(xs)
      }
    } catch applyArgumentExHandler(opt.shortDescription.capitalize, arg)

}
