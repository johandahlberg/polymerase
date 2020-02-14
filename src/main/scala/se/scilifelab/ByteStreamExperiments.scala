package se.scilifelab

import java.io.DataInputStream
import java.io.BufferedInputStream
import java.io.BufferedOutputStream
import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets
import java.io.PrintWriter
import scala.io.Source
import se.scilifelab.polymerase.PackageEncoder
import java.nio.ByteBuffer

object ByteStreamExperiments extends App {

//  // TODO This might be worth exploring further to see if using the
//  //      java input/output streams would be a better interface alternativ. They should
//  //      be able to deal with pretty much any type of input...
//  val exampleString =
//    "THIASHDDDDDDDDASLMWOEVMEWOPVMOPWEMVPOWEMVWOPEMVPWOEMVevweovmwpeovmweovmWEVM"
//  val input = new ByteArrayInputStream(
//    exampleString.getBytes(StandardCharsets.UTF_8)
//  )
//
//  import se.scilifelab.polymerase.Encodables.intEncodable
//
//  val iterator = LazyList.continually(input.read()).takeWhile(_ != -1).iterator
//  val encodedString = PackageEncoder.decode(PackageEncoder.encode(iterator, 10))
//  println(new String(encodedString.toArray.map(_.toByte)))

}
