import Lib.PitchClass

namespace MusicCompositionLang

abbrev PitchClassSet := List PitchClass

instance : Inhabited PitchClassSet where
  default := [
    PitchClass.zero,
    PitchClass.two,
    PitchClass.four,
    PitchClass.five,
    PitchClass.seven,
    PitchClass.nine,
    PitchClass.eleven
  ]

def PitchClassSet.fromPitchClasses (l : List PitchClass) : PitchClassSet := l

def PitchClassSet.toPitchClasses (s : PitchClassSet) : List PitchClass := s

def PitchClassSet.get (s : PitchClassSet) (index: Nat) : PitchClass :=
  s |>.get! (index % (s.length))

instance : ToString PitchClassSet where
  toString s := toString s.toPitchClasses

