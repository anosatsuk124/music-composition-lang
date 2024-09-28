import Lib.PitchClass

namespace MusicCompositionLang

abbrev Scale := List PitchClass

instance : Inhabited Scale where
  default := [
    PitchClass.zero,
    PitchClass.two,
    PitchClass.four,
    PitchClass.five,
    PitchClass.seven,
    PitchClass.nine,
    PitchClass.eleven
  ]

def Scale.fromPitchClasses (l : List PitchClass) : Scale := l

def Scale.toPitchClasses (s : Scale) : List PitchClass := s

def Scale.get (index: Nat) (s : Scale) : PitchClass :=
  s |>.get! (index % s.length)

instance : ToString Scale where
  toString s := toString s.toPitchClasses

