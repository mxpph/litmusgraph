import java.io.File
import parsley.{ Failure as ParseFailure, Parsley, Result, Success as ParseSuccess }
import parsley.combinator.option
import parsley.errors.combinator.ErrorMethods
import parsley.Parsley.{ atomic, notFollowedBy, some }
import scala.util.{ Failure, Success }
import Ast.*
import Lexer.{ fully, ident, integer, nonSharedIdent, sharedIdent }
import Lexer.implicits.implicitSymbol

object Parser:
  def parse(input: String) = parser.parse(input)
  def parse(input: File)   = parser.parseFile(input) match {
    case Failure(exception) => throw exception
    case Success(result)    => result
  }

  private lazy val parser  = fully(program)
  private lazy val program = Program(some(thread))
  private lazy val thread  = Thread("[" ~> ident <~ "]", some(event))
  private lazy val event   =
    Write(sharedLocation <~ ":=", value) <|>
      ReadOrUpdate(nonSharedLocation <~ ":=", sharedLocation <+> update, "//" ~> value) <|>
      ("mfence" as MemoryFence) <|>
      ("sfence" as StoreFence)

  private lazy val update: Parsley[UpdateOperation] =
    CompareAndSwap("CAS" ~> "(" ~> sharedLocation, "," ~> value, "," ~> value <~ ")")
      <|> FetchAndAdd(atomic("FAA") ~> "(" ~> sharedLocation, "," ~> value <~ ")")
      <|> FetchAndInc("FAI" ~> "(" ~> sharedLocation <~ ")")
  private lazy val sharedLocation    = SharedLocation(sharedIdent)
  private lazy val nonSharedLocation = NonSharedLocation(nonSharedIdent)
  private lazy val value             = integer
