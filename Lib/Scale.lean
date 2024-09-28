import Lib.Pitch

namespace MusicCompositionLang

abbrev Scale := List Pitch

instance : Inhabited Scale where
  default := [
    Pitch.zero,
    Pitch.two,
    Pitch.four,
    Pitch.five,
    Pitch.seven,
    Pitch.nine,
    Pitch.eleven
  ]

def Scale.fromPitches (l : List Pitch) : Scale := l

def Scale.toPitches (s : Scale) : List Pitch := s

def Scale.get (s : Scale) (index: Nat) : Pitch :=
  s |>.get! (index % (s.length))

