import org.scalatest.flatspec.AnyFlatSpec
import Ast.*
import parsley.{Failure, Success}

class MessagePassingSpec extends AnyFlatSpec {

  val messagePassing = """[t1]
                          |x := 42
                          |y := 1
                          |
                          |[t2]
                          |a := y // 1
                          |b := x // 0""".stripMargin

  val messagePassingProg = Program(
    List(
      Thread(
        "t1",
        List(
          Write(SharedLocation("x"), 42),
          Write(SharedLocation("y"), 1),
        ),
      ),
      Thread(
        "t2",
        List(
          Read(SharedLocation("y"), 1)(NonSharedLocation("a")),
          Read(SharedLocation("x"), 0)(NonSharedLocation("b")),
        ),
      ),
    ),
  )

  "Message passing" should "be parsed correctly" in:
    val result = Parser.parse(messagePassing)
    result match
        case Success(x) => ()
        case Failure(msg) => println(msg)
    assert(result.isSuccess)
    val program = result.get
    assert(program == messagePassingProg)

  // TODO: it should "not be SC-consistent" in:
  // TODO: it should "not be TSO-consistent" in:
  // TODO: it should "not be RA-consistent" in:
  // TODO: it should "be PSO-consistent" in:
  // TODO: it should "be COH-consistent" in:

}
