package dsl.filesystem

import freedsl.dsl._

object FileSystem {

  object Path {
    implicit def stringToPath(path: String) = Path(new java.io.File(path))
    implicit def fileToPath(file: java.io.File) = Path(file)
  }
  case class Path(path: java.io.File) extends AnyVal

  def interpreter = new Interpreter[Id] {
    def interpret[_] = {
      case list(p) => Right(p.path.listFiles.toVector.map(Path.apply))
    }
  }

}


@dsl trait FileSystem[M[_]] {
  def list(path: FileSystem.Path): M[Vector[FileSystem.Path]]
}


