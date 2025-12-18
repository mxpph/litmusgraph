import parsley.generic.{ ParserBridge1, ParserBridge2, ParserBridge3, ParserBridge4 }
import parsley.Parsley
import parsley.generic.ParserBridge0

object Ast:
  case class Value(val value: Int)
  object Value extends ParserBridge1[Int, Value]

  case class Location(val identifier: String, val priv: Boolean)
  object Location extends ParserBridge1[String, Location]:
    override def apply(identifier: String): Location =
      val firstChar = identifier.charAt(0)
      Location(identifier, firstChar >= 'a' && firstChar <= 'm')

  sealed trait Event

  case class Read(val to: Location, val from: Location, val value: Value) extends Event

  case class Write(val to: Location, val value: Value) extends Event

  sealed trait UpdateOperation
  case class FetchAndAdd(val loc: Location, val increment: Value) extends UpdateOperation
  object FetchAndAdd extends ParserBridge2[Location, Value, FetchAndAdd]

  object FetchAndInc extends ParserBridge1[Location, FetchAndAdd]:
    def apply(loc: Location): FetchAndAdd = FetchAndAdd(loc, Value(1))

  case class CompareAndSwap(val loc: Location, val expected: Value, val newValue: Value)
      extends UpdateOperation
  object CompareAndSwap extends ParserBridge3[Location, Value, Value, CompareAndSwap]

  case class Update(
      val loc: Location,
      val readValue: Value,
      val writeValue: Value,
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
      case Left(value)                       => Write(to, value) // e.g. x := 1
      case Right(locOrUo, value @ Value(vo)) =>
        locOrUo match
          case Left(from)    => Read(to, from, value) // e.g. a := x // 2
          case Right(update) =>
            val newValue = update match
              case FetchAndAdd(loc, Value(inc))               => Value(vo + inc)
              case CompareAndSwap(loc, Value(exp), Value(vn)) => Value(if exp == vo then vn else vo)
            Update(to, value, newValue) // e.g. a := FAA(x, 1) // 0

  sealed trait Fence      extends Event
  case object MemoryFence extends Fence, ParserBridge0[MemoryFence.type]
  case object StoreFence  extends Fence, ParserBridge0[StoreFence.type]

  case class Thread(ident: String, po: Vector[Event])
  object Thread extends ParserBridge2[String, Vector[Event], Thread]

  case class Program(threads: Vector[Thread])
  object Program extends ParserBridge1[Vector[Thread], Program]
