import parsley.generic.{ ParserBridge0, ParserBridge1, ParserBridge2, ParserBridge3, ParserBridge4 }
import parsley.Parsley
import parsley.Parsley.pure
import parsley.errors.combinator.fail

object Ast:
  type Value = Int

  case class Location(ident: String):
    def isLocal: Boolean =
      val firstChar = ident.charAt(0)
      firstChar >= 'a' && firstChar <= 'm'
  object Location extends ParserBridge1[String, Location]

  sealed trait Event

  case class Read(from: Location, value: Value)(val to: Location) extends Event
  case class Write(to: Location, value: Value)                    extends Event

  sealed trait UpdateOperation:
    val loc: Location

  case class FetchAndAdd(loc: Location, increment: Value) extends UpdateOperation
  object FetchAndAdd extends ParserBridge2[Location, Value, FetchAndAdd]

  object FetchAndInc extends ParserBridge1[Location, FetchAndAdd]:
    override def apply(loc: Location): FetchAndAdd = FetchAndAdd(loc, 1)

  case class CompareAndSwap(loc: Location, expected: Value, newValue: Value) extends UpdateOperation
  object CompareAndSwap extends ParserBridge3[Location, Value, Value, CompareAndSwap]

  case class Update(
      loc: Location,
      readValue: Value,
      writeValue: Value,
  ) extends Event
  object Update extends ParserBridge3[Location, Value, Value, Update]

  object Event
      extends ParserBridge2[
        Location,
        Either[Value, (Either[Location, UpdateOperation], Value)],
        Event,
      ]:
    override def apply(
        to: Location,
        arg2: Either[Value, (Either[Location, UpdateOperation], Value)],
    ): Event = arg2 match
      case Left(value)              => Write(to, value) // e.g. x := 1
      case Right(locOrUo, oldValue) =>
        locOrUo match
          case Left(from)    => Read(from, oldValue)(to) // e.g. a := x // 2
          case Right(update) =>
            val newValue = update match
              case FetchAndAdd(_, increment)    => oldValue + increment
              case CompareAndSwap(_, exp, swap) => if exp == oldValue then swap else oldValue
            Update(to, oldValue, newValue) // e.g. a := FAA(x, 1) // 0

    override def apply(
      to: Parsley[Location],
      arg2: => Parsley[Either[Value, (Either[Location, UpdateOperation], Value)]]
    ): Parsley[Event] =
      val verifiedTo = arg2.flatMap {
      case Left(_) => to.filter(!_.isLocal)
      case Right((locOrUo, _)) => locOrUo match
        case Left(_) => to.filter(_.isLocal)
        case Right(_) => to.filter(_.isLocal)
      }
      val verifiedArg2 = arg2.filter {
        case Left(_) => true
        case Right((locOrUo, _)) => locOrUo match
          case Left(readFrom) => !readFrom.isLocal
          case Right(update) => !update.loc.isLocal
      }
      super.apply(verifiedTo, verifiedArg2)

  sealed trait Fence      extends Event
  case object MemoryFence extends Fence, ParserBridge0[MemoryFence.type]
  case object StoreFence  extends Fence, ParserBridge0[StoreFence.type]

  case class Thread(ident: String, po: List[Event])
  object Thread extends ParserBridge2[String, List[Event], Thread]

  case class Program(threads: List[Thread])
  object Program extends ParserBridge1[List[Thread], Program]
