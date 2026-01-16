import parsley.character.satisfy
import parsley.lift.lift2
import parsley.token.{ Basic, Lexer => ParsleyLexer }
import parsley.token.descriptions.*
import parsley.token.errors.{ ErrorConfig, Label, LabelWithExplainConfig }
import parsley.Parsley

object Lexer:
  private val desc = LexicalDesc.plain.copy(
    nameDesc = NameDesc.plain.copy(
      identifierStart = Basic(c => c.isLetter),
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

  val ident          = lexer.lexeme.names.identifier
  val sharedIdent    = satisfy(c => c.isLower && c > 'm').map(_.toString) <~ lexer.space.whiteSpace
  val nonSharedIdent = satisfy(c => c.isLower && c <= 'm').map(_.toString) <~ lexer.space.whiteSpace
  val integer        = lexer.lexeme.integer.decimal32

  // Implicit symbol parsers
  val implicits = lexer.lexeme.symbol.implicits
