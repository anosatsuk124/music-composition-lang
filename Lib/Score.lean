import Lib.Note

namespace MusicCompositionLang

abbrev InScaleScore : Type := List InScaleNote

abbrev Score : Type := List Note

def Score.mk : List Note -> Score
  | ns => ns

def InScaleScore.toScore : InScaleScore -> Score
  | [] => []
  | n::(ns:InScaleScore) => n.toNote :: ns.toScore

def Score.toInScaleScore : Score -> Scale -> InScaleScore
  | [], _ => []
  | n::(ns:Score), scale => InScaleNote.mk n scale :: ns.toInScaleScore scale

