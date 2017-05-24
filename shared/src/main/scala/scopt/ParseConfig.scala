package scopt

trait ParseConfig {
  def errorOnUnknownArgument: Boolean
  def reportError(msg: String): Unit
  def reportWarning(msg: String): Unit
  def showUsageOnError: Boolean
  def showUsageAsError(usage: String): Unit
  def showTryHelp[C](helpOptions: List[ParserDef[_, C]]): Unit
  def renderingMode: RenderingMode
}

object ParseConfig {
  def apply(): ParseConfig = new ParseConfig {
    def errorOnUnknownArgument = true

    def reportError(msg: String): Unit = {
      Console.err.println("Error: " + msg)
    }

    def reportWarning(msg: String): Unit = {
      Console.err.println("Warning: " + msg)
    }

    def showUsageOnError: Boolean = true

    def showUsageAsError(usage: String): Unit = {
      Console.err.println(usage)
    }

    def showTryHelp[C](helpOptions: List[ParserDef[_, C]]): Unit = {
      def oxford(xs: List[String]): String = xs match {
        case a :: b :: Nil => a + " or " + b
        case _             => (xs.dropRight(2) :+ xs.takeRight(2).mkString(", or ")).mkString(", ")
      }
      Console.err.println("Try " + oxford(helpOptions.toList map {_.fullName}) + " for more information.")
    }

    def renderingMode: RenderingMode = RenderingMode.TwoColumns
  }
}
