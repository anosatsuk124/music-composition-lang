import Batteries.Data.Rat.Basic

namespace Measure

structure Measure where
  n: Rat
  m: Rat
deriving Repr

instance : ToString Measure where
  toString m := toString m.n ++ "/" ++ toString m.m

def toRatio : Measure -> Rat
  | {n, m} => n / m

def measure : Rat -> Rat -> Measure
  | n, m => {n := n, m := m}

