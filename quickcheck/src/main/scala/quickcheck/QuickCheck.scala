package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("minOfTwo") = forAll { (a: Int, b: Int) =>
    val h = insert (b, insert(a, empty) )
    findMin(h) == a.min(b)
  }

  property("deleteEmpty") = forAll { a: Int =>
    val h = insert(a, empty)
    deleteMin(h) == empty
  }

  property("sortedSequence") = forAll { h:H =>
    if(isEmpty(h))
      true
    else
      isSortedSequence(deleteMin(h), findMin(h))
  }

  property("minOfMeldMinOfBoth") = forAll { (h1:H, h2:H) =>
    val h = meld(h1, h2)
    if(isEmpty(h1) && isEmpty(h2))
      true
    else {
      val min = findMin(h)
      if(isEmpty(h1))
        min == findMin(h2)
      else if(isEmpty(h2))
        min == findMin(h1)
      else
        min == findMin(h1).min(findMin(h2))
    }
  }

  property("delete") = forAll { (a: Int, b: Int, c: Int) =>
    val h = insert(c, insert(b, insert(a, empty)))
    findMin(deleteMin(h)) == List(a, b, c).sorted.drop(1).head
  }

  def isSortedSequence(h:H, v:Int):Boolean = {
    if(isEmpty(h))
      true
    else {
      val v1 = findMin(h)
      v <= v1 && isSortedSequence(deleteMin(h), v1)
    }
  }

  lazy val genHeap: Gen[H] =  for {
    v <- arbitrary[Int]
    h <- oneOf(const(empty),genHeap)
  } yield (insert(v,h))

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
