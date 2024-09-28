import Lib.Score
import Lib.Mml

namespace MusicCompositionLang

syntax "m{" term "|" term* "}" :term
syntax "m{" term* "}" :term

macro_rules
  | `(m{ $note:term $notes:term* }) =>
      `(
        Score.mk
          ([$note, $notes,*]|>.map (fun x => Note.Pitch (Pitch.fromNat x) default))
          )
  | `(m{ $measure:term | $note:term $notes:term* }) =>
      `(
        Score.mk
          ([$note, $notes,*]|>.map (fun x => Note.Pitch (Pitch.fromNat x) (Duration.num $measure)))
        )

syntax term "in" term :term

macro_rules
  | `($score in $scale) =>
      `(let score :List Note := $score; InScaleScore.mk score $scale)

syntax "s{" term* "}": term

macro_rules
  | `(s{$pitch:term $pitches:term*}) =>
      `([$pitch,$pitches,*] |>.map Pitch.fromInt |> Scale.fromPitches)

#eval (m{ 0 1 2 3 4 5 6 7 } in s{(-3) (-3) 0 2 4 5 7}) |> InScaleScore.toScore |>.toMmlCode
#check m{ 0 1 2 3 4 5 6 7 } in s{(-3) (-3) 0 2 4 5 7}

#eval m{ 4 | 0 1 3 }
#check m{ 4 | 0 1 3 }

#eval m{ 4 | 0 1 3 } in s{0 2 4 5 6}
#check m{ 4 | 0 1 3 } in s{0 2 4 5 6} 

