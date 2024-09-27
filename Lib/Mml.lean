import Lib.Note
import Lib.Pitch
import Lib.Score

namespace MusicCompositionLang

abbrev Mml : Type := String

def Pitch.toMmlCode : Pitch -> Mml
  | Pitch.Lower p => "<" ++ Pitch.toMmlCode p ++ ">"
  | Pitch.Upper p => ">" ++ Pitch.toMmlCode p ++ "<"
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


def Note.toMmlCode : Note -> Mml
  | Note.Pitch p => p.toMmlCode
  | Note.Rest => "r"
  | Note.MmlCode code => code
  | Note.InScale scale p =>
    let p' := Pitch.toNat p
    scale.get! p' |>.toNat |> Pitch.fromNat |>.toMmlCode

def Mml.fromMmlCode : Mml -> Option Note
  | "c-" => Note.Pitch $ Pitch.Lower Pitch.eleven
  | "c" => Note.Pitch $ Pitch.zero
  | "c#" => Note.Pitch $ Pitch.one
  | "d-" => Note.Pitch $ Pitch.one
  | "d" => Note.Pitch $ Pitch.two
  | "d#" => Note.Pitch $ Pitch.three
  | "e-" => Note.Pitch $ Pitch.three
  | "e" => Note.Pitch $ Pitch.four
  | "e#" => Note.Pitch $ Pitch.five
  | "f-" => Note.Pitch $ Pitch.four
  | "f" => Note.Pitch $ Pitch.five
  | "f#" => Note.Pitch $ Pitch.six
  | "g-" => Note.Pitch $ Pitch.six
  | "g" => Note.Pitch $ Pitch.seven
  | "g#" => Note.Pitch $ Pitch.eight
  | "a-" => Note.Pitch $ Pitch.eight
  | "a" => Note.Pitch $ Pitch.nine
  | "a#" => Note.Pitch $ Pitch.ten
  | "b-" => Note.Pitch $ Pitch.ten
  | "b" => Note.Pitch $ Pitch.eleven
  | "b#" => Note.Pitch $ Pitch.Upper Pitch.zero
  | "r" => Note.Rest
  | _ => none

def Score.toMmlCode : Score -> Mml
  | [] => ""
  | note :: rest => note.toMmlCode ++ Score.toMmlCode rest

