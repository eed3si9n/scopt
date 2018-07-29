import minitest._
import java.util.{Calendar, GregorianCalendar}
import java.io.{ByteArrayOutputStream, File}
import java.net.{ URI, InetAddress }
import scala.concurrent.duration.Duration
import scopt.Parser

object MonadicParserSpec extends SimpleTestSuite with PowerAssertions {
  val NL = System.getProperty("line.separator")
  val builder = scopt.Builder[Config]

  test("programName(s) should generate usage text") {
    assert(programName1.usage ==
      """Usage: scopt
        |
        |""".stripMargin)
    ()
  }

  test("head(s, ...) should generate usage text") {
    assert(head1.usage == """scopt 3.x
                              |""".stripMargin)
    ()
  }

  test("head(s, ...) should compose using ++") {
    val p = head1 ++ head2
    assert(p.usage ==
      """scopt 3.x
        |x y
        |""".stripMargin)
    ()
  }

  test("head(s, ...) should compose using for comprehension") {
    val p: Parser[Unit, Config] = for {
      _ <- programName1
      _ <- head1
    } yield ()
    assert(p.usage ==
      """scopt 3.x
        |Usage: scopt
        |
        |""".stripMargin)
    ()
  }

  test("unit parser should generate usage") {
    assert(unitParser1.usage ==
      """scopt 3.x
        |Usage: scopt
        |
        |  -f, --foo  """.stripMargin)
    ()
  }

  test("unit parser should parse ()") {
    unitParser("--foo")
    unitParser("-f")
  }

  test("""for {
    |  _ <- opt[Unit]('a', "alice")
    |  _ <- opt[Unit]('b', "bob")
    |  _ <- opt[Unit]("alicebob") abbr("ab") action { x => x }
    |} yield ()""".stripMargin + " should generate usage") {
    assert(groupParser1.usage ==
      """scopt 3.x
        |Usage: scopt
        |
        |  -a, --alice      
        |  -b, --bob        
        |  -ab, --alicebob  """.stripMargin)
    ()
  }

  test("grouped parser should parse ()") {
    groupParser("-ab")
    groupParser("-abab")
  }

  test("int parser should generate usage") {
    assert(intParser1.usage ==
      """scopt 3.x
        |Usage: scopt
        |
        |  -f, --foo <value>  """.stripMargin)
    ()
  }

  test("int parser should parse 1") {
    intParser("--foo", "1")
    intParser("--foo:1")
    intParser("--foo=1")
    intParser("-f", "1")
    intParser("-f:1")
    intParser("-f=1")
    intParser("--foo", "0x01")
    intParser("--foo:0x01")
    intParser("--foo=0x01")
    intParser("-f", "0x1")
    intParser("-f:0x1")
    intParserFail{"--foo"}
    intParserFail("--foo", "bar")
    intParserFail("--foo=bar")
  }

  test("long parser should generate usage") {
    assert(longParser1.usage ==
      """scopt 3.x
        |Usage: scopt
        |
        |  -f, --foo <value>  """.stripMargin)
    ()
  }

  test("long parser should parse 1") {
    longParser("--foo", "1")
    longParser("--foo:1")
    longParser("--foo=1")
    longParser("-f", "1")
    longParser("-f:1")
    longParser("-f=1")
    longParser("--foo", "0x01")
    longParser("--foo:0x01")
    longParser("--foo=0x01")
    longParser("-f", "0x1")
    longParser("-f:0x1")
  }

  test("string parser generate usage") {
    assert(stringParser1.usage ==
      """scopt 3.x
        |Usage: scopt
        |
        |  -f, --foo <value>  """.stripMargin)
    ()
  }

  test("string parser should parse bar") {
    stringParser("--foo", "bar")
    stringParser("--foo:bar")
    stringParser("--foo=bar")
  }

  test("char parser generate usage") {
    assert(charParser1.usage ==
      """scopt 3.x
        |Usage: scopt
        |
        |  -f, --foo <value>  """.stripMargin)
    ()
  }

  test("char parser should parse 'b'") {
    charParser("--foo", "b")
    charParser("--foo:b")
    charParser("--foo=b")
    charParserFail("--foo", "bar")
    charParserFail("--foo=bar")
  }

  test("double parser should generate usage") {
    assert(doubleParser1.usage ==
      """scopt 3.x
        |Usage: scopt
        |
        |  -f, --foo <value>  """.stripMargin)
    ()
  }

  test("double parser should parse 1.0") {
    doubleParser("--foo", "1.0")
    doubleParser("--foo:1.0")
    doubleParser("--foo=1.0")
    doubleParserFail("--foo", "bar")
    doubleParserFail("--foo=bar")
  }

  test("boolean parser should generate usage") {
    assert(boolParser1.usage ==
      """scopt 3.x
        |Usage: scopt
        |
        |  -f, --foo <value>  """.stripMargin)
    ()
  }

  test("boolean parser should parse true") {
    trueParser("--foo", "true")
    trueParser("--foo:true")
    trueParser("--foo=true")
    trueParser("--foo", "1")
    trueParser("--foo:1")
    boolParserFail("--foo", "bar")
    boolParserFail("--foo=bar")
  }

  test("BigDecimal parser should generate usage") {
    assert(boolParser1.usage ==
      """scopt 3.x
        |Usage: scopt
        |
        |  -f, --foo <value>  """.stripMargin)
    ()
  }

  test("BigDecimal parser should parse 1.0") {
    bigDecimalParser("--foo", "1.0")
    bigDecimalParser("--foo=1.0")
    bigDecimalParserFail("--foo", "bar")
    bigDecimalParserFail("--foo=bar")
  }

  // """opt[Calendar]("foo") action { x => x }""" should "generate usage" in {
  //   calendarParser1.usage shouldEqual
  //     """scopt 3.x
  //       |Usage: scopt
  //       |
  //       |  -f, --foo <value>  """.stripMargin
  // }

  // it should "parse 2000-01-01 out of --foo 2000-01-01" in calendarParser("--foo", "2000-01-01")
  // it should "parse 2000-01-01 out of --foo=2000-01-01" in calendarParser("--foo=2000-01-01")
  // it should "fail to parse --foo bar"                  in calendarParserFail("--foo", "bar")
  // it should "fail to parse --foo=bar"                  in calendarParserFail("--foo=bar")

  // """opt[File]("foo") action { x => x }""" should "generate usage" in {
  //   fileParser1.usage shouldEqual
  //     """scopt 3.x
  //       |Usage: scopt
  //       |
  //       |  -f, --foo <value>  """.stripMargin
  // }

  // it should "parse test.txt out of --foo test.txt" in fileParser("--foo", "test.txt")
  // it should "parse test.txt out of --foo=test.txt" in fileParser("--foo=test.txt")

  // """opt[URI]("foo") action { x => x }""" should "generate usage" in {
  //   uriParser1.usage shouldEqual
  //     """scopt 3.x
  //       |Usage: scopt
  //       |
  //       |  -f, --foo <value>  """.stripMargin
  // }

  // it should "parse http://github.com/ out of --foo http://github.com/" in uriParser("--foo", "http://github.com/")
  // it should "parse http://github.com/ out of --foo=http://github.com/" in uriParser("--foo=http://github.com/")

  // """opt[InetAddress]("foo") action { x => x }""" should "generate usage" in {
  //   inetAddressParser1.usage shouldEqual
  //     """scopt 3.x
  //       |Usage: scopt
  //       |
  //       |  -f, --foo <value>  """.stripMargin
  // }

  // it should "parse 8.8.8.8 out of --foo 8.8.8.8" in inetAddressParser("--foo", "8.8.8.8")
  // it should "parse 8.8.8.8 out of --foo=8.8.8.8" in inetAddressParser("--foo=8.8.8.8")

  test("Duration parser should generate usage") {
    assert(durationParser1.usage ==
      """scopt 3.x
        |Usage: scopt
        |
        |  -f, --foo <value>  """.stripMargin)
    ()
  }

  test("Duration parser should parse a Duration") {
    durationParser("--foo", "30s")
    durationParser("--foo=30s")
  }

  // """opt[(String, Int)]("foo") action { x => x }""" should "generate usage" in {
  //   pairParser1.usage shouldEqual
  //     """scopt 3.x
  //       |Usage: scopt
  //       |
  //       |  --foo:<key>=<value>  """.stripMargin
  // }

  // it should """parse ("k", 1) out of --foo k=1""" in pairParser("--foo", "k=1")
  // it should """parse ("k", 1) out of --foo:k=1""" in pairParser("--foo:k=1")
  // it should """parse ("k", 1) out of --foo=k=1""" in pairParser("--foo=k=1")
  // it should """fail to parse --foo"""             in pairParserFail("--foo")
  // it should """fail to parse --foo bar"""         in pairParserFail("--foo", "bar")
  // it should """fail to parse --foo k=bar"""       in pairParserFail("--foo", "k=bar")
  // it should """fail to parse --foo=k=bar"""       in pairParserFail("--foo=k=bar")

  // """opt[Seq[Int]]("foo") action { x => x }""" should "generate usage" in {
  //   seqParser1.usage shouldEqual
  //     """scopt 3.x
  //       |Usage: scopt
  //       |
  //       |  --foo <value>  """.stripMargin
  // }

  // it should """parse Seq(1,2,3) out of --foo "1,2,3"""" in seqParser("--foo","1,2,3")
  // it should """parse Seq(1,2,3) out of "--foo=1,2,3"""" in seqParser("--foo=1,2,3")
  // it should """fail to parse --foo"""                   in seqParserFail("--foo")

  // """opt[Map[String,Boolean]]("foo") action { x => x }""" should "generate usage" in {
  //   mapParser1.usage shouldEqual
  //     """scopt 3.x
  //       |Usage: scopt
  //       |
  //       |  --foo <value>  """.stripMargin
  // }

  // it should """parse Map("true" -> true, "false" -> false) out of --foo "true=true,false=false"""" in mapParser("--foo","true=true,false=false")
  // it should """parse Map("true" -> true, "false" -> false) out of "--foo=true=true,false=false"""" in mapParser("--foo=true=true,false=false")
  // it should """fail to parse --foo"""                                                              in mapParserFail("foo")

  // """opt[Seq[(String,String)]]("foo") action { x => x }""" should "generate usage" in {
  //   seqTupleParser1.usage shouldEqual
  //     """scopt 3.x
  //       |Usage: scopt
  //       |
  //       |  --foo <value>  """.stripMargin
  // }

  // it should """parse Map("key" -> "1", "key" -> "2") out of --foo "key=1,false=false"""" in seqTupleParser("--foo","key=1,key=2")
  // it should """fail to parse --foo"""                                                    in seqTupleParserFail("foo")

  // """opt[String]("foo").required() action { x => x }""" should "fail to parse Nil" in {
  //   val result = requireParser1.parse(Nil, Config())
  //   result should ===(None)
  // }

  // """opt[Unit]("debug").hidden() action { x => x }""" should "parse () out of --debug" in {
  //   val result = unitParser2.parse(List("--debug"), Config())
  //   result.get.debug should ===(true)
  // }

  // """unknown options""" should "fail to parse by default" in intParserFail("-z", "bar")

  // """opt[(String, Int)]("foo") action { x => x } validate { x =>
  //   |  if (x > 0) success else failure("Option --foo must be >0") }""".stripMargin should "fail to parse --foo 0" in {
  //   val result = validParser1.parse(List("--foo", "0"), Config())
  //   result should ===(None)
  // }

  // """for {
  //   |  _ <- opt[Unit]('f', "foo") action { x => x }
  //   |  _ <- checkConfig { c => if (c.flag) success else failure("flag is false") }
  //   |} yield ()""".stripMargin should "generate usage" in {
  //   checkParser1.usage shouldEqual
  //     """scopt 3.x
  //       |Usage: scopt
  //       |
  //       |  -f, --foo  """.stripMargin
  // }

  // it should "fail to parse --foo 0" in checkSuccess("--foo")
  // it should "fail to parse empty"   in checkFail()

  // """arg[Int]("<port>") action { x => x }""" should "generate usage" in {
  //   intArgParser1.usage shouldEqual
  //     """scopt 3.x
  //       |Usage: scopt
  //       |
  //       |  <port>  """.stripMargin
  // }

  // it should "parse 80 out of 80" in {
  //   val result = intArgParser1.parse(List("80"), Config())
  //   result.get.intValue should ===(80)
  // }

  // it should "be required and should fail to parse Nil" in {
  //   val result = intArgParser1.parse(Nil, Config())
  //   result should ===(None)
  // }

  // """for {
  //   |  _ <- arg[String]("<a>")
  //   |  _ <- arg[String]("<b>")
  //   |} yield ()""".stripMargin should """parse "b" out of a b""" in {
  //   val result = multipleArgsParser1.parse(List("a", "b"), Config())
  //   assert((result.get.a == "a") && (result.get.b == "b"))
  // }

  // """for {
  //   |  _ <- arg[String]("<a>").action( (x, c) => c.copy(a = x) ).unbounded().optional()
  //   |  _ <- arg[String]("<b>").action( (x, c) => c.copy(b = x) ).optional()
  //   |} yield ()""".stripMargin should "generate usage" in {
  //   unboundedArgsParser1.usage shouldEqual
  //     """scopt 3.x
  //       |Usage: scopt
  //       |
  //       |  <a>  
  //       |  <b>  """.stripMargin
  // }

  // it should """parse "b" out of a b""" in {
  //   val result = unboundedArgsParser1.parse(List("a", "b"), Config())
  //   assert((result.get.a == "b") && (result.get.b == ""))
  // }

  // it should "parse nothing out of Nil" in {
  //   val result = unboundedArgsParser1.parse(Nil, Config())
  //   assert((result.get.a == "") && (result.get.b == ""))
  // }

  // """cmd("update").action({ x => x })
  //   |  .children(for {
  //   |    _ <- opt[Unit]("foo") action { x => x}
  //   |  } yield ())""".stripMargin should "generate usage" in {
  //   cmdParser1.usage shouldEqual
  //     """scopt 3.x
  //       |Usage: scopt
  //       |
  //       |  <a>  
  //       |  <b>  """.stripMargin
  // }

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

  def unitParser(args: String*): Unit = {
    val result = unitParser1.parse(args.toSeq, Config())
    assert(result.get.flag == true)
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

  def groupParser(args: String*): Unit = {
    val result = groupParser1.parse(args.toSeq, Config())
    assert(result.get.flag)
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

  def intParser(args: String*): Unit = {
    val result = intParser1.parse(args.toSeq, Config())
    assert(result.get.intValue == 1)
  }
  def intParserFail(args: String*): Unit = {
    val result = intParser1.parse(args.toSeq, Config())
    assert(result == None)
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

  def longParser(args: String*): Unit = {
    val result = intParser1.parse(args.toSeq, Config())
    assert(result.get.intValue == 1)
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

  def stringParser(args: String*): Unit = {
    val result = stringParser1.parse(args.toSeq, Config())
    assert(result.get.stringValue == "bar")
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

  def charParser(args: String*): Unit = {
    val result = charParser1.parse(args.toSeq, Config())
    assert(result.get.charValue == 'b')
  }

  def charParserFail(args: String*): Unit = {
    val result = charParser1.parse(args.toSeq, Config())
    assert(result == None)
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

  def doubleParser(args: String*): Unit = {
    val result = doubleParser1.parse(args.toSeq, Config())
    assert(result.get.doubleValue == 1.0)
  }

  def doubleParserFail(args: String*): Unit = {
    val result = doubleParser1.parse(args.toSeq, Config())
    assert(result == None)
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

  def trueParser(args: String*): Unit = {
    val result = boolParser1.parse(args.toSeq, Config())
    assert(result.get.boolValue)
  }

  def boolParserFail(args: String*): Unit = {
    val result = boolParser1.parse(args.toSeq, Config())
    assert(result == None)
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

  def bigDecimalParser(args: String*): Unit = {
    val result = bigDecimalParser1.parse(args.toSeq, Config())
    assert(result.get.bigDecimalValue == BigDecimal("1.0"))
  }

  def bigDecimalParserFail(args: String*): Unit = {
    val result = bigDecimalParser1.parse(args.toSeq, Config())
    assert(result == None)
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

  // def calendarParser(args: String*) = {
  //   val result = calendarParser1.parse(args.toSeq, Config())
  //   result.get.calendarValue.getTime should ===(new GregorianCalendar(2000, Calendar.JANUARY, 1).getTime)
  // }

  // def calendarParserFail(args: String*) = {
  //   val result = calendarParser1.parse(args.toSeq, Config())
  //   result should ===(None)
  // }

  // lazy val fileParser1: Parser[Unit, Config] = {
  //   import builder._
  //   for {
  //     _ <- programName("scopt")
  //     _ <- head("scopt", "3.x")
  //     _ <- opt[File]('f', "foo").action( (x, c) => c.copy(fileValue = x) )
  //     // _ <_ help("help")
  //   } yield ()
  // }

  // def fileParser(args: String*) = {
  //   val result = fileParser1.parse(args.toSeq, Config())
  //   result.get.fileValue should ===(new File("test.txt"))
  // }

  // lazy val uriParser1: Parser[Unit, Config] = {
  //   import builder._
  //   for {
  //     _ <- programName("scopt")
  //     _ <- head("scopt", "3.x")
  //     _ <- opt[URI]('f', "foo").action( (x, c) => c.copy(uriValue = x) )
  //     // _ <_ help("help")
  //   } yield ()
  // }

  // def uriParser(args: String*) = {
  //   val result = uriParser1.parse(args.toSeq, Config())
  //   result.get.uriValue should ===(new URI("http://github.com/"))
  // }

  // lazy val inetAddressParser1: Parser[Unit, Config] = {
  //   import builder._
  //   for {
  //     _ <- programName("scopt")
  //     _ <- head("scopt", "3.x")
  //     _ <- opt[InetAddress]('f', "foo").action( (x, c) => c.copy(inetAddressValue = x) )
  //     // _ <_ help("help")
  //   } yield ()
  // }

  // def inetAddressParser(args: String*) = {
  //   val result = inetAddressParser1.parse(args.toSeq, Config())
  //   result.get.inetAddressValue should ===(InetAddress.getByName("8.8.8.8"))
  // }

  lazy val durationParser1: Parser[Unit, Config] = {
    import builder._
    for {
      _ <- programName("scopt")
      _ <- head("scopt", "3.x")
      _ <- opt[Duration]('f', "foo").action( (x, c) => c.copy(durationValue = x) )
      // _ <_ help("help")
    } yield ()
  }

  def durationParser(args: String*): Unit = {
    val result = durationParser1.parse(args.toSeq, Config())
    assert(result.get.durationValue.toMillis == 30000L)
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

  def pairParser(args: String*): Unit = {
    val result = pairParser1.parse(args.toSeq, Config())
    assert((result.get.key == "k") && (result.get.intValue == 1))
  }

  def pairParserFail(args: String*): Unit = {
    val result = pairParser1.parse(args.toSeq, Config())
    assert(result == None)
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

  // def seqParser(args: String*) = {
  //   val result = seqParser1.parse(args.toSeq, Config())
  //   result.get.seqInts should ===(Seq(1,2,3))
  // }

  // def seqParserFail(args: String*) = {
  //   val result = seqParser1.parse(args.toSeq, Config())
  //   result should ===(None)
  // }

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

  // def mapParser(args: String*) = {
  //   val result = mapParser1.parse(args.toSeq, Config())
  //   result.get.mapStringToBool should ===(Map("true" -> true,"false" -> false))
  // }

  // def mapParserFail(args: String*) = {
  //   val result = mapParser1.parse(args.toSeq, Config())
  //   result should ===(None)
  // }

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

  // def seqTupleParser(args: String*) = {
  //   val result = seqTupleParser1.parse(args.toSeq, Config())
  //   result.get.seqTupleStringString should ===(List("key" -> "1","key" -> "2"))
  // }

  // def seqTupleParserFail(args: String*) = {
  //   val result = seqTupleParser1.parse(args.toSeq, Config())
  //   result should ===(None)
  // }

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

  // def checkSuccess(args: String*) = {
  //   val result = checkParser1.parse(args.toSeq, Config())
  //   result.get.flag should ===(true)
  // }

  // def checkFail(args: String*) = {
  //   val result = checkParser1.parse(args.toSeq, Config())
  //   result should ===(None)
  // }

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

  lazy val unboundedArgsParser1: Parser[Unit, Config] = {
    import builder._
    for {
      _ <- programName("scopt")
      _ <- head("scopt", "3.x")
      _ <- arg[String]("<a>").action( (x, c) => c.copy(a = x) ).unbounded().optional()
      _ <- arg[String]("<b>").action( (x, c) => c.copy(b = x) ).optional()
    } yield ()
  }

  lazy val cmdParser1: Parser[Unit, Config] = {
    import builder._
    for {
      _ <- programName("scopt")
      _ <- head("scopt", "3.x")
      _ <- cmd("update").action( (x, c) => c.copy(flag = true) )
             .children(for {
               _ <- opt[Unit]("foo").action( (x, c) => c.copy(stringValue = "foo") )
             } yield ())
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
