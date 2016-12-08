package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    a <- arbitrary[Int]
    h <- oneOf(Gen.const(empty), genHeap)
  } yield insert(a, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)
  
  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("insTwoElem") = forAll { (a: Int,b: Int) =>
    val h1 = insert(a, empty)
    val h2 = insert(b, h1)
    findMin(h2) == (if (a > b) b else a)
  }

  property("insDelMin") = forAll { a: Int =>
    val h = insert(a, empty)
    val h2 = deleteMin(h)
    h2 == empty
  }

  property("meldMin") = forAll { (h1: H, h2: H) =>
    val min1 = findMin(h1)
    val min2 = findMin(h2)
    val meldH   = meld(h1,h2)
    val minMeld = findMin(meldH)
    minMeld == min1 || minMeld == min2
  }

  property("minRec") = forAll { h: H =>
    def meldRec(heap: H, list: List[Int]): List[Int] = {
      if (isEmpty(heap)) list
      else meldRec(deleteMin(heap), findMin(heap):: list)
    }
    val xs = meldRec(h, List())
    xs == xs.sorted.reverse
  }

  property("meldMinMove") = forAll { (h1: H, h2: H) =>
    def meldRec(heap: H, list: List[Int]): List[Int] = {
      if (isEmpty(heap)) list
      else meldRec(deleteMin(heap), findMin(heap):: list)
    }
    val meld1 = meld(h1,h2)
    val min1  = findMin(h1)
    val meld2 = meld(deleteMin(h1),insert(min1,h2))
    val xs1   = meldRec(meld1, List())
    val xs2   = meldRec(meld2, List())
    xs1 == xs2
  }

}
