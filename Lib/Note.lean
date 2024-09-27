import Lib.Pitch
import Lib.Scale

namespace MusicCompositionLang

inductive Note where
  | MmlCode (code: String)
  | Rest
  | Pitch (pitch: Pitch)
  | InScale (scale: Scale) (inScale: InScalePitch)
deriving Inhabited

