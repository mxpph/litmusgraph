import java.io.File

import Parser.parse
import parsley.Success
import parsley.Failure

@main def main(filepaths: String*): Unit =
  for path <- filepaths do
    parse(File(path)) match
      case Success(prog) => pprint.pprintln(prog)
      case Failure(msg)  =>
        println(s"Error when parsing $path")
        println(msg)
