package scilifelab.polymerase.examples

import scilifelab.polymerase.DNACodec

object EncodingExample extends App {

  val s = "Hello World!"
  println(s"Encoding data: $s")
  // Not sure if it should be length * 8 bits or length * 16 bits here,
  // there seems to be references to the JVM using 2 bytes to represent
  // on char. The point is that that the DNA representation should use
  // one base per 2 bits.
  println(
    s"Approx. size of data: ${s.length * 8} bits"
  )

  val encoded = DNACodec.encode(s.toStream.map(_.toByte)).toList
  println(s"Encoded as DNA: ${encoded.mkString}")
  println(s"Assumed size of DNA encoded data: ${encoded.length} bases")

  val decoded = DNACodec.decode(encoded)
  println(s"Decoded data: ${decoded.map(_.toChar).mkString}")

}
