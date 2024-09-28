import Batteries.Data.Rat.Basic
import Lib.Pitch
import Lib.Scale

namespace MusicCompositionLang

abbrev Duration := Option Rat

inductive Note where
  | MmlCode (code: String)
  | Rest (duration: Duration)
  | Pitch (pitch: Pitch) (duration: Duration)
  | InScale (scale: Scale) (inScale: InScalePitch) (duration: Duration)
deriving Inhabited

