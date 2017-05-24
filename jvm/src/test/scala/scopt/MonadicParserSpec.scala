import org.scalatest._
import java.util.{Calendar, GregorianCalendar}
import java.io.{ByteArrayOutputStream, File}
import java.net.{ URI, InetAddress }
import scala.concurrent.duration.Duration
import scopt.Parser

class MonadicParserSpec extends FlatSpec with Matchers {
  val NL = System.getProperty("line.separator")
  val builder = scopt.Builder[Config]

  "programName(s)" should "generate usage text" in {
    programName1.usage shouldEqual
      """Usage: scopt
        |
        |""".stripMargin
  }

  "head(s, ...)" should "generate usage text" in {
    head1.usage shouldEqual """scopt 3.x
                              |""".stripMargin
  }

  it should "compose using ++" in {
    val p = head1 ++ head2
    p.usage shouldEqual
      """scopt 3.x
        |x y
        |""".stripMargin
  }

  it should "compose using for comprehension" in {
    val p: Parser[Unit, Config] = for {
      _ <- programName1
      _ <- head1
    } yield ()
    p.usage shouldEqual
      """scopt 3.x
        |Usage: scopt
        |
        |""".stripMargin
  }

  """opt[Unit]('f', "foo").action({ (x, c) => c })""" should "generate usage" in {
    unitParser1.usage shouldEqual
      """scopt 3.x
        |Usage: scopt
        |
        |  -f, --foo  """.stripMargin
  }

  it should "parse () out of --foo" in unitParser("--foo")
  it should "parse () out of -f" in unitParser("-f")

  """for {
    |  _ <- opt[Unit]('a', "alice")
    |  _ <- opt[Unit]('b', "bob")
    |  _ <- opt[Unit]("alicebob") abbr("ab") action { x => x }
    |} yield ()""".stripMargin should "generate usage" in {
    groupParser1.usage shouldEqual
      """scopt 3.x
        |Usage: scopt
        |
        |  -a, --alice      
        |  -b, --bob        
        |  -ab, --alicebob  """.stripMargin
  }

  it should "parse () out of -ab" in {
    groupParser("-ab")
  }

  it should "parse () out of -abab" in {
    groupParser("-abab")
  }

  """opt[Int]('f', "foo") action { x => x }""" should "generate usage" in {
    intParser1.usage shouldEqual
      """scopt 3.x
        |Usage: scopt
        |
        |  -f, --foo <value>  """.stripMargin
  }

  it should "parse 1 out of --foo 1"    in intParser("--foo", "1")
  it should "parse 1 out of --foo:1"    in intParser("--foo:1")
  it should "parse 1 out of --foo=1"    in intParser("--foo=1")
  it should "parse 1 out of -f 1"       in intParser("-f", "1")
  it should "parse 1 out of -f:1"       in intParser("-f:1")
  it should "parse 1 out of -f=1"       in intParser("-f=1")
  it should "parse 1 out of --foo 0x01" in intParser("--foo", "0x01")
  it should "parse 1 out of --foo:0x01" in intParser("--foo:0x01")
  it should "parse 1 out of --foo=0x01" in intParser("--foo=0x01")
  it should "parse 1 out of -f 0x1"     in intParser("-f", "0x1")
  it should "parse 1 out of -f:0x1"     in intParser("-f:0x1")
  it should "fail to parse --foo"       in intParserFail("--foo")
  it should "fail to parse --foo bar"   in intParserFail("--foo", "bar")
  it should "fail to parse --foo=bar"   in intParserFail("--foo=bar")

  """opt[Long]('f', "foo") action { x => x }""" should "generate usage" in {
    longParser1.usage shouldEqual
      """scopt 3.x
        |Usage: scopt
        |
        |  -f, --foo <value>  """.stripMargin
  }

  it should "parse 1 out of --foo 1"    in longParser("--foo", "1")
  it should "parse 1 out of --foo:1"    in longParser("--foo:1")
  it should "parse 1 out of --foo=1"    in longParser("--foo=1")
  it should "parse 1 out of -f 1"       in longParser("-f", "1")
  it should "parse 1 out of -f:1"       in longParser("-f:1")
  it should "parse 1 out of -f=1"       in longParser("-f=1")
  it should "parse 1 out of --foo 0x01" in longParser("--foo", "0x01")
  it should "parse 1 out of --foo:0x01" in longParser("--foo:0x01")
  it should "parse 1 out of --foo=0x01" in longParser("--foo=0x01")
  it should "parse 1 out of -f 0x1"     in longParser("-f", "0x1")
  it should "parse 1 out of -f:0x1"     in longParser("-f:0x1")

  """opt[String]("foo") action { x => x }""" should "generate usage" in {
    stringParser1.usage shouldEqual
      """scopt 3.x
        |Usage: scopt
        |
        |  -f, --foo <value>  """.stripMargin
  }

  it should """parse "bar" out of --foo bar""" in stringParser("--foo", "bar")
  it should """parse "bar" out of --foo:bar""" in stringParser("--foo:bar")
  it should """parse "bar" out of --foo=bar""" in stringParser("--foo=bar")

  """opt[Char]("foo") action { x => x }""" should "generate usage" in {
    charParser1.usage shouldEqual
      """scopt 3.x
        |Usage: scopt
        |
        |  -f, --foo <value>  """.stripMargin
  }

  it should "parse 'b' out of --foo b" in charParser("--foo", "b")
  it should "parse 'b' out of --foo:b" in charParser("--foo:b")
  it should "parse 'b' out of --foo=b" in charParser("--foo=b")
  it should "fail to parse --foo bar"  in charParserFail("--foo", "bar")
  it should "fail to parse --foo=bar"  in charParserFail("--foo=bar")

  """opt[Double]("foo") action { x => x }""" should "generate usage" in {
    doubleParser1.usage shouldEqual
      """scopt 3.x
        |Usage: scopt
        |
        |  -f, --foo <value>  """.stripMargin
  }

  it should "parse 1.0 out of --foo 1.0" in doubleParser("--foo", "1.0")
  it should "parse 1.0 out of --foo:1.0" in doubleParser("--foo:1.0")
  it should "parse 1.0 out of --foo=1.0" in doubleParser("--foo=1.0")
  it should "fail to parse --foo bar"    in doubleParserFail("--foo", "bar")
  it should "fail to parse --foo=bar"    in doubleParserFail("--foo=bar")

  """opt[Boolean]("foo") action { x => x }""" should "generate usage" in {
    boolParser1.usage shouldEqual
      """scopt 3.x
        |Usage: scopt
        |
        |  -f, --foo <value>  """.stripMargin
  }

  it should "parse true out of --foo true" in trueParser("--foo", "true")
  it should "parse true out of --foo:true" in trueParser("--foo:true")
  it should "parse true out of --foo=true" in trueParser("--foo=true")
  it should "parse true out of --foo 1"    in trueParser("--foo", "1")
  it should "parse true out of --foo:1"    in trueParser("--foo:1")
  it should "fail to parse --foo bar"      in boolParserFail("--foo", "bar")
  it should "fail to parse --foo=bar"      in boolParserFail("--foo=bar")

  """opt[BigDecimal]("foo") action { x => x }""" should "generate usage" in {
    boolParser1.usage shouldEqual
      """scopt 3.x
        |Usage: scopt
        |
        |  -f, --foo <value>  """.stripMargin
  }

  it should "parse 1.0 out of --foo 1.0" in bigDecimalParser("--foo", "1.0")
  it should "parse 1.0 out of --foo=1.0" in bigDecimalParser("--foo=1.0")
  it should "fail to parse --foo bar"    in bigDecimalParserFail("--foo", "bar")
  it should "fail to parse --foo=bar"    in bigDecimalParserFail("--foo=bar")

  """opt[Calendar]("foo") action { x => x }""" should "generate usage" in {
    calendarParser1.usage shouldEqual
      """scopt 3.x
        |Usage: scopt
        |
        |  -f, --foo <value>  """.stripMargin
  }

  it should "parse 2000-01-01 out of --foo 2000-01-01" in calendarParser("--foo", "2000-01-01")
  it should "parse 2000-01-01 out of --foo=2000-01-01" in calendarParser("--foo=2000-01-01")
  it should "fail to parse --foo bar"                  in calendarParserFail("--foo", "bar")
  it should "fail to parse --foo=bar"                  in calendarParserFail("--foo=bar")

  """opt[File]("foo") action { x => x }""" should "generate usage" in {
    fileParser1.usage shouldEqual
      """scopt 3.x
        |Usage: scopt
        |
        |  -f, --foo <value>  """.stripMargin
  }

  it should "parse test.txt out of --foo test.txt" in fileParser("--foo", "test.txt")
  it should "parse test.txt out of --foo=test.txt" in fileParser("--foo=test.txt")

  """opt[URI]("foo") action { x => x }""" should "generate usage" in {
    uriParser1.usage shouldEqual
      """scopt 3.x
        |Usage: scopt
        |
        |  -f, --foo <value>  """.stripMargin
  }

  it should "parse http://github.com/ out of --foo http://github.com/" in uriParser("--foo", "http://github.com/")
  it should "parse http://github.com/ out of --foo=http://github.com/" in uriParser("--foo=http://github.com/")

  """opt[InetAddress]("foo") action { x => x }""" should "generate usage" in {
    inetAddressParser1.usage shouldEqual
      """scopt 3.x
        |Usage: scopt
        |
        |  -f, --foo <value>  """.stripMargin
  }

  it should "parse 8.8.8.8 out of --foo 8.8.8.8" in inetAddressParser("--foo", "8.8.8.8")
  it should "parse 8.8.8.8 out of --foo=8.8.8.8" in inetAddressParser("--foo=8.8.8.8")

  """opt[Duration]("foo") action { x => x }""" should "generate usage" in {
    durationParser1.usage shouldEqual
      """scopt 3.x
        |Usage: scopt
        |
        |  -f, --foo <value>  """.stripMargin
  }

  it should "parse 30s out of --foo 30s" in durationParser("--foo", "30s")
  it should "parse 30s out of --foo=30s" in durationParser("--foo=30s")

  """opt[(String, Int)]("foo") action { x => x }""" should "generate usage" in {
    pairParser1.usage shouldEqual
      """scopt 3.x
        |Usage: scopt
        |
        |  --foo:<key>=<value>  """.stripMargin
  }

  it should """parse ("k", 1) out of --foo k=1""" in pairParser("--foo", "k=1")
  it should """parse ("k", 1) out of --foo:k=1""" in pairParser("--foo:k=1")
  it should """parse ("k", 1) out of --foo=k=1""" in pairParser("--foo=k=1")
  it should """fail to parse --foo"""             in pairParserFail("--foo")
  it should """fail to parse --foo bar"""         in pairParserFail("--foo", "bar")
  it should """fail to parse --foo k=bar"""       in pairParserFail("--foo", "k=bar")
  it should """fail to parse --foo=k=bar"""       in pairParserFail("--foo=k=bar")

  """opt[Seq[Int]]("foo") action { x => x }""" should "generate usage" in {
    seqParser1.usage shouldEqual
      """scopt 3.x
        |Usage: scopt
        |
        |  --foo <value>  """.stripMargin
  }

  it should """parse Seq(1,2,3) out of --foo "1,2,3"""" in seqParser("--foo","1,2,3")
  it should """parse Seq(1,2,3) out of "--foo=1,2,3"""" in seqParser("--foo=1,2,3")
  it should """fail to parse --foo"""                   in seqParserFail("--foo")

  """opt[Map[String,Boolean]]("foo") action { x => x }""" should "generate usage" in {
    mapParser1.usage shouldEqual
      """scopt 3.x
        |Usage: scopt
        |
        |  --foo <value>  """.stripMargin
  }

  it should """parse Map("true" -> true, "false" -> false) out of --foo "true=true,false=false"""" in mapParser("--foo","true=true,false=false")
  it should """parse Map("true" -> true, "false" -> false) out of "--foo=true=true,false=false"""" in mapParser("--foo=true=true,false=false")
  it should """fail to parse --foo"""                                                              in mapParserFail("foo")

  """opt[Seq[(String,String)]]("foo") action { x => x }""" should "generate usage" in {
    seqTupleParser1.usage shouldEqual
      """scopt 3.x
        |Usage: scopt
        |
        |  --foo <value>  """.stripMargin
  }

  it should """parse Map("key" -> "1", "key" -> "2") out of --foo "key=1,false=false"""" in seqTupleParser("--foo","key=1,key=2")
  it should """fail to parse --foo"""                                                    in seqTupleParserFail("foo")

  """opt[String]("foo").required() action { x => x }""" should "fail to parse Nil" in {
    val result = requireParser1.parse(Nil, Config())
    result should ===(None)
  }

  """opt[Unit]("debug").hidden() action { x => x }""" should "parse () out of --debug" in {
    val result = unitParser2.parse(List("--debug"), Config())
    result.get.debug should ===(true)
  }

  """unknown options""" should "fail to parse by default" in intParserFail("-z", "bar")

  """opt[(String, Int)]("foo") action { x => x } validate { x =>
    |  if (x > 0) success else failure("Option --foo must be >0") }""".stripMargin should "fail to parse --foo 0" in {
    val result = validParser1.parse(List("--foo", "0"), Config())
    result should ===(None)
  }

  """for {
    |  _ <- opt[Unit]('f', "foo") action { x => x }
    |  _ <- checkConfig { c => if (c.flag) success else failure("flag is false") }
    |} yield ()""".stripMargin should "generate usage" in {
    checkParser1.usage shouldEqual
      """scopt 3.x
        |Usage: scopt
        |
        |  -f, --foo  """.stripMargin
  }

  it should "fail to parse --foo 0" in checkSuccess("--foo")
  it should "fail to parse empty"   in checkFail()

  """arg[Int]("<port>") action { x => x }""" should "generate usage" in {
    intArgParser1.usage shouldEqual
      """scopt 3.x
        |Usage: scopt
        |
        |  <port>  """.stripMargin
  }

  it should "parse 80 out of 80" in {
    val result = intArgParser1.parse(List("80"), Config())
    result.get.intValue should ===(80)
  }

  it should "be required and should fail to parse Nil" in {
    val result = intArgParser1.parse(Nil, Config())
    result should ===(None)
  }

  """for {
    |  _ <- arg[String]("<a>")
    |  _ <- arg[String]("<b>")
    |} yield ()""".stripMargin should """parse "b" out of a b""" in {
    val result = multipleArgsParser1.parse(List("a", "b"), Config())
    assert((result.get.a == "a") && (result.get.b == "b"))
  }

  lazy val programName1: Parser[Unit, Config] = {
    import builder._
    programName("scopt")
  }

  lazy val head1: Parser[Unit, Config] = {
    import builder._
    head("scopt", "3.x")
  }

  lazy val head2: Parser[Unit, Config] = {
    import builder._
    head("x", "y")
  }

  lazy val unitParser1: Parser[Unit, Config] = {
    import builder._
    for {
      _ <- programName("scopt")
      _ <- head("scopt", "3.x")
      _ <- opt[Unit]('f', "foo") action { (x, c: Config) => c.copy(flag = true) }
    } yield ()
  }

  def unitParser(args: String*) = {
    val result = unitParser1.parse(args.toSeq, Config())
    result.get.flag should ===(true)
  }

  lazy val unitParser2: Parser[Unit, Config] = {
    import builder._
    for {
      _ <- programName("scopt")
      _ <- head("scopt", "3.x")
      _ <- opt[Unit]('f', "foo") action { (x, c: Config) => c.copy(flag = true) }
      _ <- opt[Unit]("debug").hidden() action{ (x, c) => c.copy(debug = true) }
    } yield ()
  }

  lazy val groupParser1: Parser[Unit, Config] = {
    import builder._
    for {
      _ <- programName("scopt")
      _ <- head("scopt", "3.x")
      _ <- opt[Unit]('a', "alice")
      _ <- opt[Unit]('b', "bob")
      _ <- opt[Unit]("alicebob").abbr("ab").action( (x, c) => c.copy(flag = true) )
      // _ <_ help("help")
    } yield ()
  }

  def groupParser(args: String*) = {
    val result = groupParser1.parse(args.toSeq, Config())
    result.get.flag should ===(true)
  }

  lazy val intParser1: Parser[Unit, Config] = {
    // override def showUsageOnError = true
    import builder._
    for {
      _ <- programName("scopt")
      _ <- head("scopt", "3.x")
      _ <- opt[Int]('f', "foo").action( (x, c) => c.copy(intValue = x) )
      // _ <_ help("help")
    } yield ()
  }

  def intParser(args: String*) = {
    val result = intParser1.parse(args.toSeq, Config())
    result.get.intValue should ===(1)
  }

  def intParserFail(args: String*) = {
    val result = intParser1.parse(args.toSeq, Config())
    result should ===(None)
  }

  lazy val longParser1: Parser[Unit, Config] = {
    // override def showUsageOnError = true
    import builder._
    for {
      _ <- programName("scopt")
      _ <- head("scopt", "3.x")
      _ <- opt[Long]('f', "foo").action( (x, c) => c.copy(longValue = x) )
      // _ <_ help("help")
    } yield ()
  }

  def longParser(args: String*) = {
    val result = longParser1.parse(args.toSeq, Config())
    result.get.longValue should ===(1)
  }

  lazy val stringParser1: Parser[Unit, Config] = {
    import builder._
    for {
      _ <- programName("scopt")
      _ <- head("scopt", "3.x")
      _ <- opt[String]('f', "foo").action( (x, c) => c.copy(stringValue = x) )
      // _ <_ help("help")
    } yield ()
  }

  def stringParser(args: String*) = {
    val result = stringParser1.parse(args.toSeq, Config())
    result.get.stringValue should ===("bar")
  }

  lazy val charParser1: Parser[Unit, Config] = {
    import builder._
    for {
      _ <- programName("scopt")
      _ <- head("scopt", "3.x")
      _ <- opt[Char]('f', "foo").action( (x, c) => c.copy(charValue = x) )
      // _ <_ help("help")
    } yield ()
  }

  def charParser(args: String*) = {
    val result = charParser1.parse(args.toSeq, Config())
    result.get.charValue should === ('b')
  }

  def charParserFail(args: String*) = {
    val result = charParser1.parse(args.toSeq, Config())
    result should === (None)
  }

  lazy val doubleParser1: Parser[Unit, Config] = {
    import builder._
    for {
      _ <- programName("scopt")
      _ <- head("scopt", "3.x")
      _ <- opt[Double]('f', "foo").action( (x, c) => c.copy(doubleValue = x) )
      // _ <_ help("help")
    } yield ()
  }

  def doubleParser(args: String*) = {
    val result = doubleParser1.parse(args.toSeq, Config())
    result.get.doubleValue should ===(1.0)
  }

  def doubleParserFail(args: String*) = {
    val result = doubleParser1.parse(args.toSeq, Config())
    result should ===(None)
  }

  lazy val boolParser1: Parser[Unit, Config] = {
    import builder._
    for {
      _ <- programName("scopt")
      _ <- head("scopt", "3.x")
      _ <- opt[Boolean]('f', "foo").action( (x, c) => c.copy(boolValue = x) )
      // _ <_ help("help")
    } yield ()
  }

  def trueParser(args: String*) = {
    val result = boolParser1.parse(args.toSeq, Config())
    result.get.boolValue should ===(true)
  }

  def boolParserFail(args: String*) = {
    val result = boolParser1.parse(args.toSeq, Config())
    result should ===(None)
  }

  lazy val bigDecimalParser1: Parser[Unit, Config] = {
    import builder._
    for {
      _ <- programName("scopt")
      _ <- head("scopt", "3.x")
      _ <- opt[BigDecimal]('f', "foo").action( (x, c) => c.copy(bigDecimalValue = x) )
      // _ <_ help("help")
    } yield ()
  }

  def bigDecimalParser(args: String*) = {
    val result = bigDecimalParser1.parse(args.toSeq, Config())
    result.get.bigDecimalValue should ===(BigDecimal("1.0"))
  }

  def bigDecimalParserFail(args: String*) = {
    val result = bigDecimalParser1.parse(args.toSeq, Config())
    result should ===(None)
  }

  lazy val calendarParser1: Parser[Unit, Config] = {
    import builder._
    for {
      _ <- programName("scopt")
      _ <- head("scopt", "3.x")
      _ <- opt[Calendar]('f', "foo").action( (x, c) => c.copy(calendarValue = x) )
      // _ <_ help("help")
    } yield ()
  }

  def calendarParser(args: String*) = {
    val result = calendarParser1.parse(args.toSeq, Config())
    result.get.calendarValue.getTime should ===(new GregorianCalendar(2000, Calendar.JANUARY, 1).getTime)
  }

  def calendarParserFail(args: String*) = {
    val result = calendarParser1.parse(args.toSeq, Config())
    result should ===(None)
  }

  lazy val fileParser1: Parser[Unit, Config] = {
    import builder._
    for {
      _ <- programName("scopt")
      _ <- head("scopt", "3.x")
      _ <- opt[File]('f', "foo").action( (x, c) => c.copy(fileValue = x) )
      // _ <_ help("help")
    } yield ()
  }

  def fileParser(args: String*) = {
    val result = fileParser1.parse(args.toSeq, Config())
    result.get.fileValue should ===(new File("test.txt"))
  }

  lazy val uriParser1: Parser[Unit, Config] = {
    import builder._
    for {
      _ <- programName("scopt")
      _ <- head("scopt", "3.x")
      _ <- opt[URI]('f', "foo").action( (x, c) => c.copy(uriValue = x) )
      // _ <_ help("help")
    } yield ()
  }

  def uriParser(args: String*) = {
    val result = uriParser1.parse(args.toSeq, Config())
    result.get.uriValue should ===(new URI("http://github.com/"))
  }

  lazy val inetAddressParser1: Parser[Unit, Config] = {
    import builder._
    for {
      _ <- programName("scopt")
      _ <- head("scopt", "3.x")
      _ <- opt[InetAddress]('f', "foo").action( (x, c) => c.copy(inetAddressValue = x) )
      // _ <_ help("help")
    } yield ()
  }

  def inetAddressParser(args: String*) = {
    val result = inetAddressParser1.parse(args.toSeq, Config())
    result.get.inetAddressValue should ===(InetAddress.getByName("8.8.8.8"))
  }

  lazy val durationParser1: Parser[Unit, Config] = {
    import builder._
    for {
      _ <- programName("scopt")
      _ <- head("scopt", "3.x")
      _ <- opt[Duration]('f', "foo").action( (x, c) => c.copy(durationValue = x) )
      // _ <_ help("help")
    } yield ()
  }

  def durationParser(args: String*) = {
    val result = durationParser1.parse(args.toSeq, Config())
    result.get.durationValue.toMillis should ===(30000L)
  }

  lazy val pairParser1: Parser[Unit, Config] = {
    import builder._
    for {
      _ <- programName("scopt")
      _ <- head("scopt", "3.x")
      _ <- opt[(String, Int)]("foo").action({
             case ((k, v), c) => c.copy(key = k, intValue = v)
           })
      // _ <_ help("help")
    } yield ()
  }

  def pairParser(args: String*) = {
    val result = pairParser1.parse(args.toSeq, Config())
    assert((result.get.key == "k") &&
      (result.get.intValue == 1))
  }

  def pairParserFail(args: String*) = {
    val result = pairParser1.parse(args.toSeq, Config())
    result should ===(None)
  }

  lazy val seqParser1: Parser[Unit, Config] = {
    import builder._
    for {
      _ <- programName("scopt")
      _ <- head("scopt", "3.x")
      _ <- opt[Seq[Int]]("foo").action({
             case (s, c) => c.copy(seqInts = s)
           })
      // _ <_ help("help")
    } yield ()
  }

  def seqParser(args: String*) = {
    val result = seqParser1.parse(args.toSeq, Config())
    result.get.seqInts should ===(Seq(1,2,3))
  }

  def seqParserFail(args: String*) = {
    val result = seqParser1.parse(args.toSeq, Config())
    result should ===(None)
  }

  lazy val mapParser1: Parser[Unit, Config] = {
    import builder._
    for {
      _ <- programName("scopt")
      _ <- head("scopt", "3.x")
      _ <- opt[Map[String,Boolean]]("foo").action({
             case (s, c) => c.copy(mapStringToBool = s)
           })
      // _ <_ help("help")
    } yield ()
  }

  def mapParser(args: String*) = {
    val result = mapParser1.parse(args.toSeq, Config())
    result.get.mapStringToBool should ===(Map("true" -> true,"false" -> false))
  }

  def mapParserFail(args: String*) = {
    val result = mapParser1.parse(args.toSeq, Config())
    result should ===(None)
  }

  lazy val seqTupleParser1: Parser[Unit, Config] = {
    import builder._
    for {
      _ <- programName("scopt")
      _ <- head("scopt", "3.x")
      _ <- opt[Seq[(String,String)]]("foo").action({
             case (s, c) => c.copy(seqTupleStringString = s)
           })
      // _ <_ help("help")
    } yield ()
  }

  def seqTupleParser(args: String*) = {
    val result = seqTupleParser1.parse(args.toSeq, Config())
    result.get.seqTupleStringString should ===(List("key" -> "1","key" -> "2"))
  }

  def seqTupleParserFail(args: String*) = {
    val result = seqTupleParser1.parse(args.toSeq, Config())
    result should ===(None)
  }

  lazy val requireParser1: Parser[Unit, Config] = {
    import builder._
    for {
      _ <- programName("scopt")
      _ <- head("scopt", "3.x")
      _ <- opt[String]("foo").required().action( (x, c) => c.copy(stringValue = x) )
      // _ <_ help("help")
    } yield ()
  }

  lazy val validParser1: Parser[Unit, Config] = {
    import builder._
    for {
      _ <- programName("scopt")
      _ <- head("scopt", "3.x")
      _ <- opt[Int]('f', "foo").action( (x, c) => c.copy(intValue = x) )
             .validate( x =>
               if (x > 0) success
               else failure("Option --foo must be >0") )
             .validate( x => failure("Just because") )
    } yield ()
  }

  lazy val checkParser1: Parser[Unit, Config] = {
    import builder._
    for {
      _ <- programName("scopt")
      _ <- head("scopt", "3.x")
      _ <- opt[Unit]('f', "foo").action( (x, c) => c.copy(flag = true) )
      _ <- checkConfig { c => if (c.flag) success else failure("flag is false") }
    } yield ()
  }

  def checkSuccess(args: String*) = {
    val result = checkParser1.parse(args.toSeq, Config())
    result.get.flag should ===(true)
  }

  def checkFail(args: String*) = {
    val result = checkParser1.parse(args.toSeq, Config())
    result should ===(None)
  }

  lazy val intArgParser1: Parser[Unit, Config] = {
    import builder._
    for {
      _ <- programName("scopt")
      _ <- head("scopt", "3.x")
      _ <- arg[Int]("<port>").action( (x, c) => c.copy(intValue = x) )
    } yield ()
  }

  lazy val multipleArgsParser1: Parser[Unit, Config] = {
    import builder._
    for {
      _ <- programName("scopt")
      _ <- head("scopt", "3.x")
      _ <- arg[String]("<a>").action( (x, c) => c.copy(a = x) )
      _ <- arg[String]("<b>").action( (x, c) => c.copy(b = x) )
    } yield ()
  }

  case class Config(flag: Boolean = false, intValue: Int = 0, longValue: Long = 0L, stringValue: String = "",
    doubleValue: Double = 0.0, boolValue: Boolean = false, debug: Boolean = false,
    bigDecimalValue: BigDecimal = BigDecimal("0.0"),
    calendarValue: Calendar = new GregorianCalendar(1900, Calendar.JANUARY, 1),
    fileValue: File = new File("."),
    uriValue: URI = new URI("http://localhost"),
    inetAddressValue: InetAddress = InetAddress.getByName("0.0.0.0"),
    durationValue: Duration = Duration("0s"),
    key: String = "", a: String = "", b: String = "",
    seqInts: Seq[Int] = Seq(),
    mapStringToBool: Map[String,Boolean] = Map(),
    seqTupleStringString: Seq[(String, String)] = Nil, charValue: Char = 0)
}
