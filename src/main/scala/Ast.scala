import parsley.errors.combinator.fail
import parsley.generic.{ ParserBridge0, ParserBridge1, ParserBridge2, ParserBridge3, ParserBridge4 }
import parsley.Parsley
import parsley.Parsley.pure

object Ast:
  type Value = Int

  sealed trait Location:
    val ident: String
    def isLocal: Boolean

  case class SharedLocation(ident: String) extends Location:
    def isLocal = true
  object SharedLocation extends ParserBridge1[String, SharedLocation]

  case class NonSharedLocation(ident: String) extends Location:
    def isLocal = false
  object NonSharedLocation extends ParserBridge1[String, NonSharedLocation]

  sealed trait Event

  case class Read(from: SharedLocation, value: Value)(val to: NonSharedLocation) extends Event
  case class Write(to: SharedLocation, value: Value)                             extends Event
  object Write extends ParserBridge2[SharedLocation, Value, Write]

  sealed trait UpdateOperation:
    val loc: SharedLocation

  case class FetchAndAdd(loc: SharedLocation, increment: Value) extends UpdateOperation
  object FetchAndAdd extends ParserBridge2[SharedLocation, Value, FetchAndAdd]

  object FetchAndInc extends ParserBridge1[SharedLocation, FetchAndAdd]:
    override def apply(loc: SharedLocation): FetchAndAdd = FetchAndAdd(loc, 1)

  case class CompareAndSwap(loc: SharedLocation, expected: Value, newValue: Value)
      extends UpdateOperation
  object CompareAndSwap extends ParserBridge3[SharedLocation, Value, Value, CompareAndSwap]

  case class Update(
      loc: SharedLocation,
      readValue: Value,
      writeValue: Value,
  ) extends Event
  object Update extends ParserBridge3[SharedLocation, Value, Value, Update]

  object ReadOrUpdate
      extends ParserBridge3[
        NonSharedLocation,
        Either[SharedLocation, UpdateOperation],
        Value,
        Read | Update,
      ]:
    override def apply(
        to: NonSharedLocation,
        arg2: Either[SharedLocation, UpdateOperation],
        readValue: Value,
    ): Read | Update =
      arg2 match
        case Left(from)      => Read(from, readValue)(to) // e.g. a := x // 2
        case Right(updateOp) =>
          val writeValue = updateOp match
            case FetchAndAdd(sloc, increment)    => readValue + increment
            case CompareAndSwap(sloc, exp, swap) =>
              if exp == readValue then swap else readValue
          Update(updateOp.loc, readValue, writeValue) // e.g. a := FAA(x, 1) // 0

  sealed trait Fence      extends Event
  case object MemoryFence extends Fence, ParserBridge0[MemoryFence.type]
  case object StoreFence  extends Fence, ParserBridge0[StoreFence.type]

  case class Thread(ident: String, po: List[Event])
  object Thread extends ParserBridge2[String, List[Event], Thread]

  case class Program(threads: List[Thread])
  object Program extends ParserBridge1[List[Thread], Program]
