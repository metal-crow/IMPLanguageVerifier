//Taken from https://gist.github.com/Daiver/0893a772d581dbf3dddda9d646a1cefb#file-graphviz-scala

import graphviz._
import sys.process._
import java.io._

package graphviz {
  package object graphio {
    def saveGraph(fname: String, pngFName: String, graph: Graph) = {
      val rendered = graph.render
      new PrintWriter(fname) { write(rendered); close }
      s"dot -Tpng ${fname} -o${pngFName}" !
    }
  }
}