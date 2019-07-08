package scilifelab.polymerase

import java.nio.CharBuffer
import java.nio.ByteBuffer
import java.io.InputStream
import java.io.DataInputStream
import java.io.EOFException

@SerialVersionUID(1)
case class DataContainer(
    index: Int,
    val data: Array[Char]
) extends Ordered[DataContainer]
    with Serializable {

  import scala.math.Ordered.orderingToOrdered
  def compare(that: DataContainer): Int = this.index compare that.index

}
