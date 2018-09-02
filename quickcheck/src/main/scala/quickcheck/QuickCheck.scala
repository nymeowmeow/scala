package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    n <- Arbitrary.arbitrary[List[Int]]
  } yield n.foldRight[H](empty)((i : Int, acc : H) => insert(i, acc))
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  //If you insert an element into an empty heap, then delete the minimum, the resulting heap should be empty.
  property("insert into empty and delete") = forAll { i : Int =>
    deleteMin(insert(i, empty)) == empty
  }

  //If you insert any two elements into an empty heap, finding the minimum of the resulting heap should get the smallest of the two elements back.
  property("insert two") = forAll { (a : Int, b : Int) =>
    val h1 = insert(a, empty)
    val h2 = insert(b, h1)

    findMin(h2) == List(a, b).min
  }

  //Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima. (Hint: recursion and helper functions are your friends.)
  property("sort sequence") = forAll { h : H =>
    def isSorted(h: H) : Boolean = {
      if (isEmpty(h)) true
      else {
        val min = findMin(h)
        val h1 = deleteMin(h)
        isEmpty(h1) || ((min <= findMin(h1)) && isSorted(h1))
      }
    }
    isSorted(h)
  }
  //Finding a minimum of the melding of any two heaps should return a minimum of one or the other.
  property("min melding") = forAll { (h1 : H, h2: H) =>
      val melded = meld(h1, h2)
      if (isEmpty(melded)) true
      else if (isEmpty(h1)) findMin(h2) == findMin(melded)
      else if (isEmpty(h2)) findMin(h1) == findMin(melded)
      else {
        findMin(melded) == List(findMin(h1), findMin(h2)).min
      }
  }

  property("empty meld") = forAll { h : H =>
    meld(h, empty) == h
  }

  def contains(h : H, i : Int) : Boolean = {
    if (isEmpty(h)) false
    else {
      if (i == findMin(h)) true
      else contains(deleteMin(h), i)
    }
  }

  property("contains") = forAll { (i : Int, h : H) =>
    val h2 = insert(i, h)
    contains(h2, i)
  }
}
