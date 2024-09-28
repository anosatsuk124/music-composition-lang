import Batteries.Data.Rat.Basic
import Lib.Pitch
import Lib.Scale

namespace MusicCompositionLang

-- abbrev Duration := Option Rat

inductive Duration where
  | num : Nat -> Duration
  | Dot : Duration -> Duration
deriving Repr

instance : Inhabited Duration where
  default := Duration.num 1

def Duration.toString : Duration -> String
    | Duration.num n => n.repr
    | Duration.Dot d => toString d ++ "."

def Duration.addDot : Duration -> Duration
    | d => Duration.Dot d

#eval Duration.toString $ Duration.Dot (Duration.num 2)
#eval Duration.toString $ Duration.num 2 |>.addDot

instance : ToString Duration where
  toString := Duration.toString

inductive Note where
  | MmlCode (code: String)
  | Rest (duration: Duration)
  | Pitch (pitch: Pitch) (duration: Duration)
deriving Inhabited, Repr

structure InScaleNote where
  note : Note
  scale : Scale
deriving Inhabited, Repr

def InScaleNote.toNote : InScaleNote -> Note := fun sn =>
  match sn.note with
    | Note.Pitch p d =>
      let n := Pitch.toNat p % 12
      let p' := sn.scale.get n |>.toNat |> Pitch.fromNat |>.upOctave p.getOctave
      Note.Pitch p' d
    | _ => sn.note

