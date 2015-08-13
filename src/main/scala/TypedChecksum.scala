package com.basic

import shapeless._, Nat._
import shapeless.ops.nat._
import shapeless.ops.hlist._

object TypedChecksum {

  trait HasChecksum[L <: HList, S <: Nat]

  implicit object hnilHasChecksum extends HasChecksum[HNil, _0]

  implicit def hlistHasChecksum[
    H <: Nat, T <: HList, S <: Nat,
    TS <: Nat, TL <: Nat,
    HP <: Nat, HS <: Nat
  ](
    implicit
    st: HasChecksum[T, TS],
    tl: Length.Aux[T, TL],
    hp: Prod.Aux[H, Succ[TL], HP],
    hs: Sum.Aux[TS, HP, HS],
    sm: Mod.Aux[HS, _11, S]
  ) = new HasChecksum[H :: T, S] {}

  def isValid[L <: HList, LENGTH <: Nat, SUM <: Nat](
    implicit
    l: Length.Aux[L, LENGTH],
    c: HasChecksum[L, SUM]
  ) = {}

  def checksum(l: List[Int]): Int = l.reverse.zipWithIndex.map {
    case (v, i) => v * (i + 1)
  }.sum % 11

  isValid[_3 :: _4 :: _5 :: _8 :: _8 :: _2 :: _8 :: _6 :: _5 :: HNil, _9, _0]
}
