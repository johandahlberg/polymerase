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
      // TODO Don't know if I should keep this or not.
      //require(
      //  -128 <= x && x < 128,
      //  s"Only integers in the range [-128, 128) can be converted to unsigned bytes. Integer was: ${x}"
      //)
      new UnsignedByte((x.toByte & 0xFF).toByte)
    }
  }

  case object Package {

    // TODO Reconsider the names of these methods. They are rather unclear.

    def fromBytes(
        inputIndex: Int,
        inputTotalNumberOfBlocks: Int,
        inputBlockLength: Int,
        inputData: Array[Byte]
    ): Package = {
      Package(
        inputIndex = inputIndex,
        inputTotalNumberOfBlocks = inputTotalNumberOfBlocks,
        inputBlockLength = inputBlockLength,
        dataLength = inputData.length,
        inputData = inputData.map(UnsignedByte(_))
      )
    }

    def fromUnsignedBytes(
        inputIndex: Int,
        inputTotalNumberOfBlocks: Int,
        inputBlockLength: Int,
        inputData: Array[UnsignedByte]
    ): Package = {
      Package(
        inputIndex = inputIndex,
        inputTotalNumberOfBlocks = inputTotalNumberOfBlocks,
        inputBlockLength = inputBlockLength,
        dataLength = inputData.length,
        inputData = inputData
      )
    }

    def fromRawBytes(
        inputData: Array[UnsignedByte]
    ): Package = {
      val dataLength = length(inputData)
      val data = inputData.drop(dataOffsett)
      Package(
        inputIndex = index(inputData),
        inputTotalNumberOfBlocks = totalNumberOfBlocks(inputData),
        inputBlockLength = inputData.drop(dataOffsett).length,
        dataLength = dataLength,
        inputData = data
      )
    }

    private val intByteLength = 4
    private val numberOfBlocksOfSet = 0 * intByteLength
    private val indexOffset = 1 * intByteLength
    private val lengthOffset = 2 * intByteLength
    private val dataOffsett = 3 * intByteLength

    def totalPackageByteLength(dataLengthInBytes: Int): Int =
      Package
        .fromBytes(
          inputIndex = 0,
          inputBlockLength = 0,
          inputTotalNumberOfBlocks = 0,
          inputData = Array.fill(dataLengthInBytes)(0)
        )
        .byteLength

    def index(bytes: Array[UnsignedByte]) =
      CodecUtils.decodeIntFromBytes(
        bytes.drop(indexOffset).take(intByteLength).map(_.underlyingByte)
      )

    def length(bytes: Array[UnsignedByte]) = {
      CodecUtils.decodeIntFromBytes(
        bytes.drop(lengthOffset).take(intByteLength).map(_.underlyingByte)
      )
    }
    def totalNumberOfBlocks(bytes: Array[UnsignedByte]) = {
      CodecUtils
        .decodeIntFromBytes(
          bytes
            .drop(numberOfBlocksOfSet)
            .take(intByteLength)
            .map(_.underlyingByte)
        )
    }
  }

  case class Package(
      private val inputIndex: Int,
      private val inputBlockLength: Int,
      private val inputTotalNumberOfBlocks: Int,
      private val dataLength: Int,
      private val inputData: Array[UnsignedByte],
      private val inputErrorCorrectionBytes: Array[UnsignedByte] = Array.empty
  ) extends Ordered[Package] {

    // TODO Don't know if the we actually need to keep the errorCorrectionBits separate
    // Probably not, so I'll most likely remove this later.

    val bytes: Array[UnsignedByte] =
      CodecUtils
        .encodeIntAsBytes(inputTotalNumberOfBlocks)
        .map(UnsignedByte(_)) ++
        CodecUtils.encodeIntAsBytes(inputIndex).map(UnsignedByte(_)) ++
        CodecUtils.encodeIntAsBytes(dataLength).map(UnsignedByte(_)) ++
        inputData.padTo(inputBlockLength, UnsignedByte(0)) ++
        inputErrorCorrectionBytes

    lazy val index = Package.index(bytes)
    lazy val length = Package.length(bytes)
    lazy val totalNumberOfBlocks = Package.totalNumberOfBlocks(bytes)
    lazy val byteLength = bytes.length

    def bytesAsIntArray: Array[Int] = {
      bytes
        .map(_.intValue)
    }

    def data = bytes.drop(Package.dataOffsett).take(length)
    def errorCorrectionBytes = bytes.drop(Package.dataOffsett).drop(length)

    override def toString(): String = {
      s"Package(totalNumberOfBlocks=$totalNumberOfBlocks, index=$index, length=$length, " +
        s"data=${data.map(_.intValue).toSeq}, errorCorrectionBytes=${errorCorrectionBytes.map(_.intValue).toSeq})"
    }

    def compare(that: Package): Int = this.index.compare(that.index)

  }

}
