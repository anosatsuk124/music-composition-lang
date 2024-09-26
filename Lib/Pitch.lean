namespace Pitch

inductive Pitch where
  | zero : Pitch
  | one : Pitch
  | two : Pitch
  | three : Pitch
  | four : Pitch
  | five : Pitch
  | six : Pitch
  | seven : Pitch
  | eight : Pitch
  | nine : Pitch
  | ten : Pitch
  | eleven : Pitch
  | Upper : Pitch -> Pitch
  | Lower : Pitch -> Pitch
deriving Repr

def toNat : Pitch -> Nat
  | Pitch.zero => 0
  | Pitch.one => 1
  | Pitch.two => 2
  | Pitch.three => 3
  | Pitch.four => 4
  | Pitch.five => 5
  | Pitch.six => 6
  | Pitch.seven => 7
  | Pitch.eight => 8
  | Pitch.nine => 9
  | Pitch.ten => 10
  | Pitch.eleven => 11
  | Pitch.Upper p => toNat p + 12
  | Pitch.Lower p => toNat p - 12

def fromNat : Nat -> Pitch
  | 0 => Pitch.zero
  | 1 => Pitch.one
  | 2 => Pitch.two
  | 3 => Pitch.three
  | 4 => Pitch.four
  | 5 => Pitch.five
  | 6 => Pitch.six
  | 7 => Pitch.seven
  | 8 => Pitch.eight
  | 9 => Pitch.nine
  | 10 => Pitch.ten
  | 11 => Pitch.eleven
  | n + 12 => Pitch.Upper (fromNat n)

def add : Pitch -> Nat -> Pitch := fun p n => fromNat (toNat p + n)

def sub : Pitch -> Nat -> Pitch := fun p n => fromNat (toNat p - n)

instance : HAdd Pitch Nat Pitch where
  hAdd := add

instance : HSub Pitch Nat Pitch where
  hSub := sub

