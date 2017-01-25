package dsl.filesystem

import java.io._
import freedsl.dsl._
import util.{Failure, Success, Try}
import cats._
import cats.syntax.all._

object FileSystem {

  object Path {
    implicit def stringToPath(path: String) = Path(new java.io.File(path))
    implicit def fileToPath(file: java.io.File) = Path(file)
  }
  case class Path(path: java.io.File) extends AnyVal {
    override def toString = path.getPath
  }

  def interpreter = new Interpreter[Id] {
    def interpret[_] = {
      case list(p) =>
        Try(p.path.listFiles.toVector.map(Path.apply)).toEither.leftMap(e => FileError(s"Error listing directory $p", e))
      case readStream(path, f) =>
        Try {
          val is = new BufferedInputStream(new FileInputStream(path.path))
          try f(is)
          finally is.close
        }.toEither.leftMap(e => FileError(s"Error reading file $path", e))
    }
  }

  case class FileError(message: String, cause: Throwable) extends Exception(message, cause) with Error

}


@dsl trait FileSystem[M[_]] {
  def list(path: FileSystem.Path): M[Vector[FileSystem.Path]]
  def readStream[T](path: FileSystem.Path, f: InputStream => T): M[T]
}


