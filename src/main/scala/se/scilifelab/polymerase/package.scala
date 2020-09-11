package se.scilifelab

package object polymerase {
  type Nucleotide = Char
  val nucleotides: Vector[Nucleotide] = Vector('A', 'T', 'C', 'G')

  class UnsignedByte(val underlyingByte: Byte) extends AnyVal {
    def intValue: Int = underlyingByte & 0xFF
  }

  implicit def unsignedByteToByte(unsignedByte: UnsignedByte): Byte =
    unsignedByte.underlyingByte

  object UnsignedByte {
    def apply(x: Byte): UnsignedByte = new UnsignedByte(x)

    def apply(x: Int): UnsignedByte = {
      require(
        -128 <= x && x < 128,
        s"Only integers in the range [-128, 128) can be converted to unsigned bytes. Integer was: ${x}"
      )
      new UnsignedByte((x.toByte & 0xFF).toByte)
    }
  }

  case object Package {

    // TODO Reconsider the names of these methods. They are rather unclear.

    def fromBytes(
        inputIndex: Int,
        inputBlockLength: Int,
        inputData: Array[Byte]
    ): Package = {
      Package(inputIndex, inputBlockLength, inputData.map(UnsignedByte(_)))
    }

    def fromRawBytes(
        inputData: Array[UnsignedByte],
        inputBlockLength: Int
    ): Package = {
      Package(
        inputIndex = index(inputData),
        inputBlockLength = inputBlockLength,
        inputData = inputData.drop(dataOffsett)
      )
    }

    val intByteLength = 4
    private val indexOffset = 0 * intByteLength
    private val lengthOffset = 1 * intByteLength
    private val dataOffsett = 2 * intByteLength

    def index(bytes: Array[UnsignedByte]) =
      CodecUtils.decodeIntFromBytes(
        bytes.drop(indexOffset).take(intByteLength).map(_.underlyingByte)
      )

    def length(bytes: Array[UnsignedByte]) =
      CodecUtils.decodeIntFromBytes(
        bytes.drop(lengthOffset).take(intByteLength).map(_.underlyingByte)
      )

  }

  case class Package(
      private val inputIndex: Int,
      private val inputBlockLength: Int,
      private val inputData: Array[UnsignedByte]
  ) {

    val bytes: Array[UnsignedByte] =
      CodecUtils.encodeIntAsBytes(inputIndex).map(UnsignedByte(_)) ++
        CodecUtils.encodeIntAsBytes(inputData.length).map(UnsignedByte(_)) ++
        inputData.padTo(inputBlockLength, UnsignedByte(0))

    def index = Package.index(bytes)
    def length = Package.length(bytes)

    def data = bytes.drop(Package.dataOffsett).take(length)

    def equals(that: Package): Boolean = {
      this.index == that.index && this.length == that.length &&
      data.sameElements(that.data)
    }

    override def toString(): String = {
      s"Package(index=$index, length=$length," +
        s"data=${data.map(_.intValue).toSeq})"
    }

  }

}
