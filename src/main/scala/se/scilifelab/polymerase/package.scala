package se.scilifelab

package object polymerase {
  type Nucleotide = Char
  val nucleotides: Vector[Nucleotide] = Vector('A', 'T', 'C', 'G')

  class UnsignedByte(val underlyingByte: Byte) extends AnyVal {
    def intValue: Int = underlyingByte & 0xFF
  }

  object UnsignedByte {
    def apply(x: Byte): UnsignedByte = new UnsignedByte(x)
  }

  trait Package {
    val index: Int
    val length: Int
    val data: Array[UnsignedByte]
  }
}
