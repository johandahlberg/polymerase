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

  trait Package {
    val index: Int
    val length: Int
    val blockLength: Int
    val inputData: Array[UnsignedByte]

    def dataToEncode: Array[UnsignedByte] = {
      inputData.padTo(blockLength, UnsignedByte(0))
    }

    def dataToDecode: Array[UnsignedByte] = {
      inputData.take(blockLength)
    }

    def equals(that: Package): Boolean = {
      this.index == that.index && this.length == that.length && dataToEncode
        .sameElements(that.dataToEncode)
    }

    override def toString(): String = {
      s"Package($index, $length, $blockLength, ${dataToDecode.map(_.intValue).toList})"
    }
  }
}
