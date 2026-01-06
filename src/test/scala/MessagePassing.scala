import org.scalatest.flatspec.AnyFlatSpec
import Ast.*

class MessagePassingSpec extends AnyFlatSpec {

  val messagePassing = """[t1]
                          |x := 42
                          |y := 1
                          |
                          |[t2]
                          |a := y // 1
                          |b := x // 0""".stripMargin

  val messagePassingProg = Program(
    Vector(
      Thread(
        "t1",
        Vector(
          Write(Location("x"), 42),
          Write(Location("y"), 1),
        ),
      ),
      Thread(
        "t2",
        Vector(
          Read(Location("y"), 1)(Location("a")),
          Read(Location("x"), 0)(Location("b")),
        ),
      ),
    ),
  )

  "Message passing" should "be parsed correctly" in:
    val result = Parser.parse(messagePassing)
    assert(result.isSuccess)
    val program = result.get
    assert(program == messagePassingProg)

  // TODO: it should "not be SC-consistent" in:
  // TODO: it should "not be TSO-consistent" in:
  // TODO: it should "not be RA-consistent" in:
  // TODO: it should "be PSO-consistent" in:
  // TODO: it should "be COH-consistent" in:

}
