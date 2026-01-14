import parsley.syntax.all
import scala.collection.mutable.{ ListBuffer, Map as MutableMap, Set as MutableSet }
import scala.util.{ boundary, Try }
import scalax.collection.generic.{ AbstractDiEdge, AnyDiEdge, MultiEdge }
import scalax.collection.immutable.Graph
import scalax.collection.OneOrMore
import Ast._

object Checker:
  type GWrite = Write | Update
  type GRead  = Read | Update

  type EventEdge = AbstractDiEdge[Event]
  case class PoEdge(src: Event, dst: Event) extends EventEdge(src, dst), MultiEdge:
    override def extendKeyBy: OneOrMore[Any] = OneOrMore.one(PoEdge)
  case class RfEdge(src: GWrite, dst: GRead) extends EventEdge(src, dst), MultiEdge:
    override def extendKeyBy: OneOrMore[Any] = OneOrMore.one(RfEdge)
  case class MoEdge(src: GWrite, dst: GWrite) extends EventEdge(src, dst), MultiEdge:
    override def extendKeyBy: OneOrMore[Any] = OneOrMore.one(MoEdge)
  case class RbEdge(src: GRead, dst: GWrite) extends EventEdge(src, dst), MultiEdge:
    override def extendKeyBy: OneOrMore[Any] = OneOrMore.one(RbEdge)

  // TODO: initialization writes are always mo-before other writes! preprocess!
  def buildFromProgram(
      program: Program,
  ): Either[String, List[Graph[Event, AbstractDiEdge[Event]]]] =
    getMetadata(program) match
      case Left(err)                                   => Left(err)
      case Right((locations, readsByLoc, writesByLoc)) =>
        val initializationWrites = locations.map(Write(_, 0))

        val initialEdges = ListBuffer.empty[EventEdge]
        for thread <- program.threads do
          val po    = thread.po
          val first = po(0)
          for initWrite <- initializationWrites do initialEdges += PoEdge(initWrite, first)
          po.sliding(2).foreach {
            case src :: dst :: Nil => initialEdges += PoEdge(src, dst)
            case _                 => ()
          }
        end for
        val allEvents = initializationWrites ++ program.threads.flatMap(_.po)
        // add rf edges (filter writes by value)
        val initialGraph = Graph.from(allEvents, initialEdges.result())
        ???
    // addEdges(initialGraph, allEvents)(using writesByLoc, readsByLoc)

  private def getMetadata(
      program: Program,
  ): Either[String, (List[Location], Map[Location, List[GRead]], Map[Location, Set[GWrite]])] =
    val locations        = ListBuffer.empty[Location]
    val readsByLocation  = MutableMap.empty[Location, ListBuffer[GRead]]
    val writesByLocation = MutableMap.empty[Location, MutableSet[GWrite]]
    val result           = boundary {
      for
        thread <- program.threads
        event  <- thread.po
      do
        val loc = event match
          case read @ Read(to, value) =>
            locations += to
            readsByLocation.getOrElseUpdate(to, ListBuffer.empty) += read
          case write @ Write(to, value) =>
            locations += to
            val set = writesByLocation.getOrElseUpdate(to, MutableSet.empty)
            set.foreach {
              case `write` | Update(_, _, `value`) =>
                boundary.break(
                  Left(s"Found multiple writes on location ${to.ident} with value ${value}"),
                )
              case _ => ()
            }
            set += write
          case update @ Update(to, readValue, writeValue) =>
            locations += to
            val writeSet = writesByLocation.getOrElseUpdate(to, MutableSet.empty)
            writeSet.foreach {
              case Write(_, `writeValue`) | Update(_, _, `writeValue`) =>
                boundary.break(
                  Left(s"Found multiple writes on location ${to.ident} with value ${writeValue}"),
                )
              case _ => ()
            }
            writeSet += update
            readsByLocation.getOrElseUpdate(to, ListBuffer.empty) += update
          case _ => ()

      val finalReadsByLocation  = readsByLocation.map((loc, buf) => loc -> buf.result()).toMap
      val finalWritesByLocation = writesByLocation.map((loc, buf) => loc -> buf.toSet).toMap
      Right((locations.result(), finalReadsByLocation, finalWritesByLocation))
    }
    result
