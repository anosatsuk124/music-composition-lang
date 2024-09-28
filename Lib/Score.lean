import Lib.Note

namespace MusicCompositionLang

abbrev InScaleScore : Type := List InScaleNote

abbrev Score : Type := List Note

def Score.mk : List Note -> Score
  | ns => ns

def InScaleScore.mk : List Note -> Scale -> InScaleScore
  | ns, scale => ns.map (fun n => InScaleNote.mk n scale)

def InScaleScore.transpose : InScaleScore -> Int -> InScaleScore
  | [], _ => []
  | n::(ns: InScaleScore), i => n.transpose i :: ns.transpose i

def Score.transpose : Score -> Int -> Score
  | [], _ => []
  | n::(ns: Score), i => n.transpose i :: ns.transpose i

def InScaleScore.toScore : InScaleScore -> Score
  | [] => []
  | n::(ns:InScaleScore) => n.toNote :: ns.toScore

def Score.toInScaleScore : Score -> Scale -> InScaleScore
  | [], _ => []
  | n::(ns:Score), scale => InScaleNote.mk n scale :: ns.toInScaleScore scale

