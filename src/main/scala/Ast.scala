import parsley.generic.{ ParserBridge0, ParserBridge1, ParserBridge2, ParserBridge3, ParserBridge4 }
import parsley.Parsley

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

  sealed trait UpdateOperation
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

  sealed trait Fence      extends Event
  case object MemoryFence extends Fence, ParserBridge0[MemoryFence.type]
  case object StoreFence  extends Fence, ParserBridge0[StoreFence.type]

  case class Thread(ident: String, po: Vector[Event])
  object Thread extends ParserBridge2[String, Vector[Event], Thread]

  case class Program(threads: Vector[Thread])
  object Program extends ParserBridge1[Vector[Thread], Program]
