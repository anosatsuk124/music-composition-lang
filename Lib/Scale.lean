import Lib.PitchClass

namespace MusicCompositionLang

abbrev Scale := List PitchClass

def Scale.fromPitchClasses (l : List PitchClass) : Scale := l

def Scale.toPitchClasses (s : Scale) : List PitchClass := s

instance : ToString Scale where
  toString s := toString s.toPitchClasses

