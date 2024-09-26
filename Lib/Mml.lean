import Lib.Pitch

namespace MusicCompositionLang

def toMmlString : Pitch -> String
  | Pitch.Lower p => "<" ++ toMmlString p ++ ">"
  | Pitch.Upper p => ">" ++ toMmlString p ++ "<"
  | Pitch.zero => "c"
  | Pitch.one => "c#"
  | Pitch.two => "d"
  | Pitch.three => "d#"
  | Pitch.four => "e"
  | Pitch.five => "f"
  | Pitch.six => "f#"
  | Pitch.seven => "g"
  | Pitch.eight => "g#"
  | Pitch.nine => "a"
  | Pitch.ten => "a#"
  | Pitch.eleven => "b"

def fromMmlString : String -> Option Pitch
  | "c-" => Pitch.Lower Pitch.eleven
  | "c" => Pitch.zero
  | "c#" => Pitch.one
  | "d-" => Pitch.one
  | "d" => Pitch.two
  | "d#" => Pitch.three
  | "e-" => Pitch.three
  | "e" => Pitch.four
  | "e#" => Pitch.five
  | "f-" => Pitch.four
  | "f" => Pitch.five
  | "f#" => Pitch.six
  | "g-" => Pitch.six
  | "g" => Pitch.seven
  | "g#" => Pitch.eight
  | "a-" => Pitch.eight
  | "a" => Pitch.nine
  | "a#" => Pitch.ten
  | "b-" => Pitch.ten
  | "b" => Pitch.eleven
  | "b#" => Pitch.Upper Pitch.zero
  | _ => none

