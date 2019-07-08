package scilifelab.polymerase

import org.scalatest._

class DataContainerSpec extends FlatSpec with Matchers {

  "A DataContainer" should "be able to be serialized and deserialized" in {
    val container = DataContainer(1, List('A', 'T', 'C', 'G'))
    val backAndForth = DataContainer.fromByteArray(container.toByteArray())
    container should be(backAndForth)
  }
}
