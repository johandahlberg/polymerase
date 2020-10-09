package se.scilifelab.polymerase.fountaincodes

import scala.util.Random
import se.scilifelab.polymerase.Package
import se.scilifelab.polymerase.UnsignedByte
import scala.collection.immutable.SortedMap

/**
  * TODO Write docs
  *
  * @param index
  * @param data
  * @param degree
  * @param neighbors
  */
case class Symbol(
    index: Int,
    data: Array[UnsignedByte],
    degree: Int,
    neighbors: Option[Set[Int]] = None
) {
  override def toString(): String =
    s"Symbol(index=$index, degree=$degree, neighbors=$neighbors)"
}

/**
  * TODO Write docs
  *
  * @param randomSeed
  */
class FountainsCodes(
    randomSeed: Int = 1234,
    packageMultiplicationFactor: Double = 3.0
) {

  /**
    * Generate degree indexes, between 0 and nbrOfBlocks
    *
    * @param index
    * @param degree
    * @param nbrOfBlocks
    * @return
    */
  private def generateIndexPositions(
      index: Int,
      degree: Int,
      nbrOfBlocks: Int
  ): Seq[Int] = {
    val random = new Random(seed = index)
    for (d <- 0 until degree) yield {
      random.between(0, nbrOfBlocks)
    }
  }

  private def xOrByteArrays(
      x: Array[UnsignedByte],
      y: Array[UnsignedByte]
  ): Array[UnsignedByte] = {
    x.zip(y).map {
      case (xe, ye) => UnsignedByte(xe ^ ye)
    }
  }

  /**
    * TODO Write docs.
    *
    * @param data
    * @return
    */
  def encode(data: Seq[Package]): Iterator[Package] = {

    val nbrOfBlocks = data.length
    val nbrOfPackages = (nbrOfBlocks * packageMultiplicationFactor).toInt

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
      val pck = Package(
        inputIndex = i,
        inputBlockLength = symbolData.length,
        dataLength = symbolData.length,
        inputData = symbolData
      )
      pck
    }

  }

  private def getDegrees(
      nbrOfBlocks: Int
  ): LazyList[Int] = {
    val solitonDist = new RobustSoliton(
      nbrOfBlocks,
      0.05,
      nbrOfBlocks / 2 + 1,
      Some(randomSeed)
    )
    1 #:: solitonDist.sample(Int.MaxValue - 1)
  }

  /**
    * For  set of input symbols, figure out based on their index, which their
    * neighbours should be. This only words of the random number generator
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

    // TODO Figure out a way to only sample degress up to the necessary
    // index, to allow lazy number of blocks.
    // One way to do it might be to use a stream.
    val degrees = getDegrees(nbrOfBlocks)

    for { pck <- packages } yield {
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

  // TODO How do we know how many blocks are to be decoded?

  /**
    * TODO Write docs!
    *
    * @param data
    * @param numberOfBlocks
    * @return
    */
  def decode(
      data: Iterator[Package],
      numberOfBlocks: Int
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

    // TODO Later, figure out how to do this in on the fly. For now pick up all the symbols
    val symbols = recoverGraph(data, numberOfBlocks).toSeq

    val iteratonInitator =
      IteratorContainer(
        symbols,
        SortedMap.empty,
        true
      )
    val iteratedSymbols = iterateSymbols(iteratonInitator)
    val blocks = iteratedSymbols.blocks

    val nbrOfSolvedBlocks = blocks.size
    val decodedPackages = blocks.values.map { block =>
      Package.fromRawBytes(block)
    }

    (decodedPackages.toSeq, nbrOfSolvedBlocks)
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
