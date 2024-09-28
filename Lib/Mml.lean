import Lib.Note
import Lib.Pitch
import Lib.Score

namespace MusicCompositionLang

abbrev Mml : Type := String

def Pitch.toMmlCode : Pitch -> Mml := fun p =>
  let rec tomml : Pitch -> Mml := fun p => match p with
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
    | _ => ""
  let rec mml : Pitch -> Mml := fun p => match p with
    | Pitch.Lower p => "<" ++ mml p ++ ">"
    | Pitch.Upper p => ">" ++ mml p ++ "<"
    | _ => tomml p

  "{" ++ mml p ++ "}"

def Pitch.fromMmlCode : Mml -> Option Pitch
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


def Note.toMmlCode : Note -> Mml
  | Note.Pitch p d => p |> Pitch.toMmlCode |>.append $ d |> toString
  | Note.Rest d => "r" |>.append $ d |> toString
  | Note.MmlCode code => code

def Score.toMmlCode : Score -> Mml
  | [] => ""
  | note :: rest => note.toMmlCode ++ Score.toMmlCode rest

