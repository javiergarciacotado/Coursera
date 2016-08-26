package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    value <- arbitrary[Int]
    heap <- oneOf(const(empty), genHeap)
  } yield insert(value, heap)
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)


  // find the minimum value in a heap
  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  // generate the minimum value for a heap
  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  // insert two element in a heap, findMin will return the smaller one
  property("min2") = forAll { (val1: Int, val2: Int) =>
    val heap = insert(val1, insert(val2, empty))
    val smaller = if(val1 < val2) val1 else val2
    findMin(heap) == smaller
  }

  // insert one into empty, delete it, then the heap will be empty
  property("empty1") = forAll { val1: Int =>
    val heap = insert(val1, empty)
    deleteMin(heap) == empty
  }


  // meld two heap, the findMin will return minimum of two heap
  property("meld1") = forAll{ (heap1: H, heap2: H) =>
    val min1 = findMin(heap1)
    val min2 = findMin(heap2)

    val meldHeap = meld(heap1, heap2)
    val meldMin = findMin(meldHeap)
    meldMin == min1 || meldMin == min2

  }

  // findMin and store, you should get a sorted seq
  property("sorted") = forAll { heap: H =>

    def heapToList(h: H, acc: List[Int]): List[Int] = {
      if(isEmpty(h)) acc
      else heapToList(deleteMin(h), acc :+ findMin(h))

    }
    val list = heapToList(heap, List())
    list == list.sorted

  }

  // test deleteMin for Bogus4BinomialHeap
  // see Bogus4BBinomialHeap, this one worked weired because the "reverse"
  property("sorted") = forAll { (val1: Int, val2: Int, val3: Int) =>

    def heapToList(h: H, acc: List[Int]): List[Int] = {
      if(isEmpty(h)) acc
      else heapToList(deleteMin(h), acc :+ findMin(h))

    }

    val sortedList = List(val1, val2, val3).sorted


    val heap = insert(val1, insert(val2, insert(val3, empty)))
    val deletedHeap = deleteMin(heap)

    heapToList(deletedHeap, List()) == sortedList.tail

  }


}