package freedsl

import java.io._

package object filesystem {

  object Path {
    implicit def stringToPath(path: String) = Path(new java.io.File(path))
    implicit def fileToPath(file: java.io.File) = Path(file)
  }

  case class Path(path: java.io.File) extends AnyVal {
    override def toString = path.getPath
  }
}
