object Nucleotide extends Enumeration {
  type Nucleotide  = Value

  val A, C, G, T = Value
}

import java.nio.file.FileAlreadyExistsException
import java.util.NoSuchElementException

import Nucleotide._

import scala.annotation.tailrec
import scala.util.Try

Nucleotide.values


type Codon = (Nucleotide, Nucleotide, Nucleotide)

type Gene = List[Codon]

val geneSequence = "ACGTGGCTCTCTAACGTACGTACGTACGGGGTTTATATATACCCTAGGACTCCCTTT"


def stringToCodon(s: String) : Option[Codon] = {
  import Nucleotide.withName

  if (s.size == 3)
    try {
      Some(
        (withName(s.charAt(0).toString),
          withName(s.charAt(1).toString),
          withName(s.charAt(2).toString))
      )
    } catch {
      case e: NoSuchElementException => None
    }

  else None
}

def stringToGene(s: String) : Gene = {

  (geneSequence grouped 3).map(stringToCodon _).toList.flatten

}

val gene = stringToGene(geneSequence)
gene

def geneContainsCodon(g: Gene, c: Codon) : Boolean = g contains c

val tgg: Codon = (T, G, G)
geneContainsCodon(gene, tgg)

val acg: Codon = (A, C, G)
geneContainsCodon(gene,acg)

val ggg: Codon = (G, G, G)
geneContainsCodon(gene, ggg)

val tta: Codon = (T, T, A)
geneContainsCodon(gene, tta)

def linearContains(g: Gene, c: Codon) : Boolean = g match {
  case Nil => false
  case c1 :: cs if c == c1 => true
  case _ :: cs => linearContains(cs, c)
}

linearContains(gene, tgg)
linearContains(gene, acg)
linearContains(gene, ggg)
linearContains(gene, tta)


type Gene2 = Array[Codon]

def stringToGene2(s: String) : Gene2 = {
  (geneSequence grouped 3).map(stringToCodon _).toArray.flatten
}

val gene2 = stringToGene2(geneSequence).sorted


def binaryContains(g: Gene2, c: Codon): Boolean = {

  @tailrec
  def aux(g: Gene2, c: Codon, lo: Int, hi: Int) : Boolean = {

    if (hi <= lo) return false

    val mid = ( hi + lo ) / 2

    println(g(mid))

    if ( g(mid) > c ) aux(g, c, lo, mid)
    else if ( g(mid) < c ) aux(g, c, mid+1, hi)
    else true
  }

  aux(g, c, 0, g.size - 1)
}

binaryContains(gene2, tgg)
binaryContains(gene2, acg)
binaryContains(gene2, ggg)
binaryContains(gene2, tta)


