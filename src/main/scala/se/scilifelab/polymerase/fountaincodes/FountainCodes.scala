package se.scilifelab.polymerase.fountaincodes

import scala.util.Random
import se.scilifelab.polymerase.Package
import se.scilifelab.polymerase.UnsignedByte
import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet

/**
  * A symbol contains data. If the degree is one, it means
  * that it contains the actual data of the original block,
  * if the degree is higher than one, it can be xor:ed with
  * an other symbol in its set of neigbours to reduce its degree.
  * Thus eventually by iterating over the symbols that original
  * data will (at a high probability) be recovered.
  * @param index The index of the symbol
  * @param data The currect data of the symbol.
  * @param degree The current degree of the symbol
  * @param neighbors The indexes of the other symbols that can be xor:ed with this one
  */
private case class Symbol(
    index: Int,
    data: Array[UnsignedByte],
    degree: Int,
    neighbors: Option[Set[Int]] = None
) {
  override def toString(): String =
    s"Symbol(index=$index, degree=$degree, neighbors=$neighbors)"
}

/**
  * This codec implements LT-codes. An excellent introduction to the topic can be found here:
  * https://franpapers.com/en/algorithmic/2018-introduction-to-fountain-codes-lt-codes-with-python/
  * This text has also been very helpful in the writing of this implementation.
  * @param randomSeed The random seed used to prime the PRNG for the Soliton Distribution.
  * @param packageMultiplicationFactor The number of packages to generate from the original data. Should should be > 1
  */
class FountainsCodes(
    randomSeed: Int = 1234,
    packageMultiplicationFactor: Double = 3.0
) {

  /**
    * Generate degree indexes, between 0 and nbrOfBlocks,
    * that are used to decide at encoding and decoding
    * to decide which blocks to xor.
    * @param index
    * @param degree
    * @param nbrOfBlocks
    * @return
    */
  private def generateIndexPositions(
      index: Int,
      degree: Int,
      nbrOfBlocks: Int
  ): LazyList[Int] = {
    val random = new Random(seed = index)
    LazyList.from(
      (0 until degree).view.map(_ => random.between(0, nbrOfBlocks))
    )
  }

  /**
    * Xor to arrays of Unsigned bytes of the same length
    * @param x
    * @param y
    * @return
    */
  private def xOrByteArrays(
      x: Array[UnsignedByte],
      y: Array[UnsignedByte]
  ): Array[UnsignedByte] = {
    x.zip(y).map {
      case (xe, ye) => UnsignedByte(xe ^ ye)
    }
  }

  /**
    * Perform the LT-encoding operation on the given packages. It will return an iterator of packages
    * where the input packages have been xor:ed together, to create N packges, there N is determined
    * by the codecs packageMultiplicationFactor.
    *
    * @param data
    * @return
    */
  def encode(data: Seq[Package]): Iterator[Package] = {

    val nbrOfBlocks = data.length
    val nbrOfPackages = (nbrOfBlocks * packageMultiplicationFactor).toInt

    System.err.println(s"Had $nbrOfBlocks blocks.")
    System.err.println(s"Made $nbrOfPackages packages.")

    val degrees = getDegrees(nbrOfBlocks)

    for { i <- Iterator.range(0, nbrOfPackages) } yield {
      val degreeOfIndex = degrees(i)

      val indexesToXOr =
        generateIndexPositions(i, degree = degreeOfIndex, nbrOfBlocks).toSet
      val symbolData =
        indexesToXOr
          .map(index => data(index))
          .map(_.bytes)
          .reduce((x, y) => xOrByteArrays(x, y))

      val pck = Package.fromUnsignedBytes(
        inputIndex = i,
        inputTotalNumberOfBlocks = nbrOfBlocks,
        inputBlockLength = symbolData.length,
        inputData = symbolData
      )
      // TODO
      //val pck = Package(
      //  inputIndex = i,
      //  inputTotalNumberOfBlocks = nbrOfBlocks,
      //  inputBlockLength = symbolData.length,
      //  dataLength = symbolData.length,
      //  inputData = symbolData
      //)
      pck
    }

  }

  /**
    * Get the for a specific block, so that they can be retrived by index later.
    * @param nbrOfBlocks
    * @return
    */
  private def getDegrees(
      nbrOfBlocks: Int
  ): LazyList[Int] = {
    val solitonDist = new RobustSoliton(
      nbrOfBlocks,
      0.05,
      nbrOfBlocks / 2 + 1,
      Some(randomSeed)
    )
    // Sample as many degress as is needed.
    1 #:: solitonDist.sample(Int.MaxValue - 1)
  }

  /**
    * For a set of input packages, figure out based on their index, which their
    * neighbours should be. This only works of the random number generator
    * used by generateIndexPosition uses the same seed.
    *
    * @param symbols
    * @param nbrOfBlocks
    * @return
    */
  private def recoverGraph(
      packages: Iterator[Package],
      nbrOfBlocks: Int
  ): Iterator[Symbol] = {

    val degrees = getDegrees(nbrOfBlocks)

    for { (pck, i) <- packages.zipWithIndex } yield {
      //System.err.println(s"Working on package nbr $i")
      //System.err.println(s"Package: $pck")

      val neighbors =
        generateIndexPositions(
          index = pck.index,
          degree = degrees(pck.index),
          nbrOfBlocks
        )
      Symbol(
        index = pck.index,
        degree = degrees(pck.index),
        data = pck.data,
        neighbors = Some(neighbors.toSet)
      )
    }
  }

  /**
    * Assumes that the set of given packages have been LT-encoded and attempts
    * to decode the original packages.
    *
    * @param data
    * @return The decoded packages, and the number of packages that have been decoded
    */
  def decode(
      data: Iterator[Package]
  ): (Seq[Package], Int) = {

    /**
      * IteratorContainer is helper class used to encapsulate the state
      * of the iteration when trying to go from the input symbols, to
      * a the blocks of data.
      * @param symbols
      * @param blocks
      * @param lastIteratorSolvedABlock
      */
    case class IteratorContainer(
        symbols: Seq[Symbol],
        blocks: SortedMap[Int, Array[UnsignedByte]],
        lastIterationSolvedABlock: Boolean
    )

    /**
      * Solve a symbol of degree one (i.e. put the data from that block into the block where it belongs),
      * and remove this symbol from the symbols remaining to be solved.
      * In order to speed up computaion, only update blocks which have not been solved yet.
      *
      * @param symbol
      * @param symbols
      * @param index
      * @param blocks
      * @return
      */
    def solveSymbol(
        symbol: Symbol,
        symbols: Seq[Symbol],
        index: Int,
        blocks: SortedMap[Int, Array[UnsignedByte]]
    ): IteratorContainer = {
      val blockIndex = symbol.neighbors.get.head
      val symbolsWithCurrentSymbolRemoved = symbols.patch(index, Nil, 1)
      val block = blocks.getOrElse(blockIndex, Array[UnsignedByte]())

      if (block.isEmpty) {
        val updatedBlocks =
          blocks.updated(blockIndex, symbol.data)
        val reducedNeighboursSymbols = reduceNeighbors(
          blockIndex,
          updatedBlocks,
          symbolsWithCurrentSymbolRemoved
        )
        IteratorContainer(reducedNeighboursSymbols, updatedBlocks, true)
      } else {
        IteratorContainer(
          symbolsWithCurrentSymbolRemoved,
          blocks,
          true
        )
      }
    }

    /**
      * Iterate across the symbols recursively until the last iteration did not solve a
      * new block. This either means that we ran out of symbols to decode, or that
      * the symbols we did have did not help us decode any additional blocks.
      *
      * @param container an IteratorContainer with the current iteration state.
      * @return the final state of the iteration.
      */
    def iterateSymbols(container: IteratorContainer): IteratorContainer = {
      if (container.lastIterationSolvedABlock) {
        val solvableSymbolAndIndex =
          container.symbols.zipWithIndex.filter {
            case (symbol, _) => symbol.degree == 1
          }.headOption

        // There was at least one symbol of degree one.
        // Go on to try to solve it.
        if (solvableSymbolAndIndex.isDefined) {
          iterateSymbols(
            solveSymbol(
              solvableSymbolAndIndex.get._1,
              container.symbols,
              solvableSymbolAndIndex.get._2,
              container.blocks
            )
          )
        } else {
          // There were no symbols of degree 1, which means that we are not able to
          // resolve any more symbols.
          IteratorContainer(container.symbols, container.blocks, false)
        }
      } else {
        // We did not solve a block in the last iteration. We will return the
        // results we have got so far.
        container
      }

    }

    /**
      * Guess the number of blocks, based on a vote from
      * the first 100 packages.
      * @param packageIterator
      * @return A guess of how many blocks need to be decoded.
      */
    def guessNumberOfBlocks(packageIterator: Iterator[Package]): Int =
      packageIterator
        .take(100)
        .map(_.totalNumberOfBlocks)
        .toSeq
        .groupMapReduce(identity)(_ => 1)(_ + _) // find the number of votes for a particular number of packages
        .maxBy(_._2) // Find the number with the most votes
        ._1

    val (dataIterator, countIterator) = data.duplicate
    val nbrOfBlocksGuess = guessNumberOfBlocks(countIterator)
    //System.err.println(s"Guess there were $nbrOfBlocksGuess blocks.")
    // TODO Later, figure out how to do this in on the fly. For now pick up all the symbols
    val symbols = recoverGraph(dataIterator, nbrOfBlocksGuess).toSeq

    val iteratonInitator =
      IteratorContainer(
        symbols,
        SortedMap.empty,
        true
      )
    val iteratedSymbols = iterateSymbols(iteratonInitator)
    val blocks = iteratedSymbols.blocks

    val decodedPackages = blocks.values
      .map { block =>
        Package.fromRawBytes(block)
      }

    // Deduplicate packages
    val distinctPackages = SortedSet.from(decodedPackages).toSeq
    val nbrOfSolvedBlocks = distinctPackages.size

    (distinctPackages, nbrOfSolvedBlocks)
  }

  /**
    * Find all symbols which refer to the block we just solved,
    * and XOr the data of those symbols with the solved block,
    * and remove it form their neighours. This will reduce the
    * degree of those symbols, hopefully producing more symbols
    * of degree one, that can be used to solve more blocks.
    *
    * @param blockIndex
    * @param blocks
    * @param symbols
    * @return the remaining symbols with the solved block removed.
    */
  private def reduceNeighbors(
      blockIndex: Int,
      blocks: Map[Int, Array[UnsignedByte]],
      symbols: Seq[Symbol]
  ): Seq[Symbol] = {
    symbols
      .map { symbol =>
        if (symbol.degree > 1 && symbol.neighbors.get.contains(blockIndex)) {
          val newSymbolData = xOrByteArrays(blocks(blockIndex), symbol.data)
          val newNeighbors =
            symbol.neighbors.get.filterNot(_ == blockIndex)
          Symbol(
            symbol.index,
            newSymbolData,
            newNeighbors.size,
            Some(newNeighbors)
          )
        } else {
          symbol
        }
      }
  }

}
