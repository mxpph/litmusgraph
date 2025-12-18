import parsley.token.{ Basic, Lexer => ParsleyLexer }
import parsley.token.descriptions.*
import parsley.token.errors.{ ErrorConfig, Label, LabelWithExplainConfig }
import parsley.Parsley

object Lexer:
  private val desc = LexicalDesc.plain.copy(
    nameDesc = NameDesc.plain.copy(
      identifierStart = Basic(c => c.isLower),
      identifierLetter = Basic(c => c.isLetterOrDigit),
    ),
    spaceDesc = SpaceDesc.plain.copy(
      lineCommentStart = "#",
      lineCommentAllowsEOF = true,
    ),
    symbolDesc = SymbolDesc.plain.copy(
      hardOperators = Set(
        ":=",
        "//",
      ),
      hardKeywords = Set(
        "CAS",
        "FAA",
        "FAI",
        "mfence",
        "sfence",
      ),
    ),
    numericDesc = NumericDesc.plain.copy(
      integerNumbersCanBeBinary = false,
      integerNumbersCanBeHexadecimal = false,
      integerNumbersCanBeOctal = false,
    ),
  )
  private val errConfig = new ErrorConfig {
    override def labelSymbol: Map[String, LabelWithExplainConfig] = Map(
      ":=" -> Label("assignment"),
      "["  -> Label("start of new thread"),
    )
  }
  val lexer = ParsleyLexer(desc, errConfig)

  def fully[A](p: Parsley[A]): Parsley[A] = lexer.fully(p)

  val ident: Parsley[String]   = lexer.lexeme.names.identifier
  val nlIdent: Parsley[String] = lexer.nonlexeme.names.identifier
  val integer: Parsley[Int]    = lexer.lexeme.integer.decimal32
  val nlInteger: Parsley[Int]  = lexer.nonlexeme.integer.decimal32

  // Implicit symbol parsers
  val implicits = lexer.lexeme.symbol.implicits
