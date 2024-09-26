namespace MusicCompositionLang

inductive PitchClass where
  | zero : PitchClass
  | one : PitchClass
  | two : PitchClass
  | three : PitchClass
  | four : PitchClass
  | five : PitchClass
  | six : PitchClass
  | seven : PitchClass
  | eight : PitchClass
  | nine : PitchClass
  | ten : PitchClass
  | eleven : PitchClass
deriving Repr

def PitchClass.toNat : PitchClass -> Nat
  | PitchClass.zero => 0
  | PitchClass.one => 1
  | PitchClass.two => 2
  | PitchClass.three => 3
  | PitchClass.four => 4
  | PitchClass.five => 5
  | PitchClass.six => 6
  | PitchClass.seven => 7
  | PitchClass.eight => 8
  | PitchClass.nine => 9
  | PitchClass.ten => 10
  | PitchClass.eleven => 11

def PitchClass.fromNat : Nat -> PitchClass
  | 0 => PitchClass.zero
  | 1 => PitchClass.one
  | 2 => PitchClass.two
  | 3 => PitchClass.three
  | 4 => PitchClass.four
  | 5 => PitchClass.five
  | 6 => PitchClass.six
  | 7 => PitchClass.seven
  | 8 => PitchClass.eight
  | 9 => PitchClass.nine
  | 10 => PitchClass.ten
  | 11 => PitchClass.eleven
  | n + 12  => fromNat n

instance : ToString PitchClass where
  toString pc := toString (PitchClass.toNat pc)

