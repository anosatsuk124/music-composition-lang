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

instance : ToString Duration where
  toString := Duration.toString

inductive Note where
  | MmlCode (code: String)
  | Rest (duration: Duration)
  | Pitch (pitch: Pitch) (duration: Duration)
deriving Inhabited, Repr

def Note.transpose : Note -> Int -> Note
  | Note.Pitch p d, n =>
    if n >= 0 then
      let n := n.natAbs
      Note.Pitch (p + n) d
    else
      let n := n.natAbs
      Note.Pitch (p - n) d
  | n, _ => n

structure InScaleNote where
  note : Note
  scale : Scale
deriving Inhabited, Repr

def InScaleNote.transpose : InScaleNote -> Int -> InScaleNote
  | n, i => InScaleNote.mk (n.note.transpose i) n.scale

def InScaleNote.toNote : InScaleNote -> Note := fun sn =>
  match sn.note with
    | Note.Pitch p d =>
      let p_nat := Pitch.toInt p
      let degree := p_nat % 12
      let p_div := p_nat / 12
      if degree >= 0 then
        let degree := degree.natAbs
        let p' := Scale.get sn.scale degree |>.upOctave p_div.natAbs
        Note.Pitch p' d
      else
        let degree := degree.natAbs
        let p' := Scale.get sn.scale degree |>.downOctave p_div.natAbs
        Note.Pitch p' d
    | _ => sn.note

