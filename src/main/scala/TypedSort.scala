package com.basic

import shapeless._
import Nat._
import HList._
import ops.nat._
import ops.hlist._
import LT._

/**
 * http://milessabin.com/blog/2012/01/27/type-level-sorting-in-shapeless/
 **/

trait LTEq[A <: Nat, B <: Nat]

object LTEq {
  import Nat._0

  type <=[A <: Nat, B <: Nat] = LTEq[A, B]

  implicit def ltEq1 = new <=[_0, _0] {}
  implicit def ltEq2[B <: Nat] = new <=[_0, Succ[B]] {}
  implicit def ltEq3[A <: Nat, B <: Nat](implicit lt: A <= B) =
    new <=[Succ[A], Succ[B]] {}
}

object SL {
  trait SelectLeast[L <: HList, M <: Nat, Rem <: HList] {
    def apply(l: L): (M, Rem)
  }

  trait LowPrioritySelectLeast {
    implicit def hlistSelectLeast1[H <: Nat, T <: HList] =
      new SelectLeast[H :: T, H, T] {
        def apply(l: H :: T): (H, T) = (l.head, l.tail)
      }
  }

  object SelectLeast extends LowPrioritySelectLeast {
    implicit def hlistSelectLeast3[H <: Nat, T <: HList, TM <: Nat, TRem <: HList]
      (implicit tsl : SelectLeast[T, TM, TRem], ev : TM < H) = new SelectLeast[H :: T, TM, H :: TRem] {
      def apply(l : H :: T) : (TM, H :: TRem) = {
        val (tm, rem) = tsl(l.tail)
        (tm, l.head :: rem)
      }
    }
  }

  def selectLeast[L <: HList, M <: Nat, Rem <: HList](l : L)(implicit sl : SelectLeast[L, M, Rem]) = sl(l)
}

object TypedSort {
  import LTEq._
  import SL._

  def typed[T](t: => T) {}

  trait NonDecreasing[L <: HList]

  implicit def hnilNonDecreasing =
    new NonDecreasing[HNil] {}

  implicit def hlistNonDecreasing1[H] =
    new NonDecreasing[H :: HNil] {}

  implicit def hlistNonDecreasing2[H1 <: Nat, H2 <: Nat, T <: HList]
    (implicit ltEq: H1 <= H2, ndt: NonDecreasing[H2 :: T]) =
      new NonDecreasing[H1 :: H2 :: T] {}

  def acceptNonDecreasing[L <: HList](l: L)
    (implicit ni: NonDecreasing[L]) = l


/* Selection sort */
  trait SelectionSort[L <: HList, S <: HList] {
    def apply(l : L) : S
  }

  trait LowPrioritySelectionSort {
    implicit def hlistSelectionSort1[S <: HList] = new SelectionSort[S, S] {
      def apply(l : S) : S = l
    }
  }

  object SelectionSort extends LowPrioritySelectionSort {
    implicit def hlistSelectionSort2[L <: HList, M <: Nat, Rem <: HList, ST <: HList]
      (implicit sl: SelectLeast[L, M, Rem], sr: SelectionSort[Rem, ST]) = new SelectionSort[L, M :: ST] {
        def apply(l: L) = {
          val (m, rem) = sl(l)
          m :: sr(rem)
        }
      }
  }

  def selectionSort[L <: HList, S <: HList](l : L)(implicit sort : SelectionSort[L, S]) = sort(l)

  val unsorted = _3 :: _1 :: _4 :: _0 :: _2 :: HNil
  val sorted = selectionSort(unsorted)
  typed[_0 :: _1 :: _2 :: _3 :: _4 :: HNil](sorted)
  acceptNonDecreasing(sorted)
}
