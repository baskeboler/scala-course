package quickcheck

import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import org.scalacheck._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {
  lazy val genHeap: Gen[H] = {
    for {
      a <- arbitrary[Int]
      h <- Gen.oneOf[H](Gen.const(empty), genHeap)
    } yield (insert(a, h))
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }
  property("insert") = forAll {
    a: Int =>
      findMin(insert(a, insert(a, empty))) == a
  }
  property("deletenly") = forAll {
    a: Int => {
      val h = insert(a, empty)
      isEmpty(deleteMin(h))
    }
  }
  property("deletenly2") = forAll {
    a: Int => {
      val h = insert(a, insert(a, empty))
      isEmpty(deleteMin(h)) == false
    }
  }
  property("deletenly") = forAll {
    a: Int => {
      val h = insert(a, empty)
      findMin(h) == a
    }
  }

  property("insert2thendeletemin") = forAll {
    (a: Int, b: Int) => {
      val h = deleteMin(insert(a, insert(b, empty)))

      if (a <= b) findMin(h) == b
      else findMin(h) == a
    }
  }

  property("insert and meld empty with singleton") = forAll {
    a: Int =>  {
      findMin(meld(insert(a, empty), empty)) == a &&
        findMin(meld(empty, insert(a, empty))) == a
    }
  }

  property("merge") = forAll {
    (h1: H, h2: H) => {
      val m: H = meld(h1, h2)
      if (!isEmpty(h1) && !isEmpty(h2)) {
        val m1: Int = findMin(h1)
        val m2: Int = findMin(h2)
        if (m1 < m2) {
          findMin(m) == m1
        } else
          findMin(m) == m2
      } else
        isEmpty(m)
    }
  }

  property("compare lists for meld") = forAll {
    (h: H, g: H) => {
      val (gList, hList) = (getList(g), getList(h))
      val consList = (gList ++ hList).sorted(ord)
      consList == getList(meld(h, g)).sorted(ord)
    }
  }

  property("merge-empty") = forAll {
    (a: Int) => {
      val b = a % 100000
      val h1 = insert(b + 1, insert(b, (insert(b + 2, empty))))
      findMin(meld(h1, empty)) == b && findMin(meld(empty, h1)) == b
    }
  }

  property("sorted") = forAll {
    h: H => {
      val l = getList(h)
      isOrdered(l)
    }
  }

  property("sorted2") = forAll {
    (h: H, g: H) => {
      val l = getList(meld(h, g))
      //println(l)
      isOrdered(l)
    }
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h))==m
  }

  property("sorted3") = forAll {
    (a: Int, b: Int) => {
      val h = insert(a, insert(b, empty))
      if (a < b) {
        getList(h) == List(a, b)
      } else getList(h) == List(b, a)
    }
  }

  property("consistent") = forAll {
    h: H => isConsistent(h)
  }
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  def getList(h: H): List[A] =
    isEmpty(h) match {
      case true => List()
      case false => findMin(h) :: getList(deleteMin(h))
    }

  def isOrdered(xs: List[A]): Boolean =
    xs == xs.sorted(ord)
  /*  xs match {
    case Nil => true
    case a :: Nil => true
    case a :: b :: rs => a <= b && isOrdered(b :: xs)
  }*/

  def isConsistent(h: H): Boolean =
     isOrdered(getList(h))


}
