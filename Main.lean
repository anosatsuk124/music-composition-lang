import MusicCompositionLang

open MusicCompositionLang

def main : IO Unit := do
    IO.println $ Score.toMmlCode $ [Note.Pitch (Pitch.Upper Pitch.zero) default, Note.Pitch Pitch.zero (Duration.num 2).Dot]
    let scale := [0, 2, 3, 5, 7, 8, 10].map (fun x => PitchClass.fromNat x)
      |> Scale.fromPitchClasses
    IO.println $ "Scale: " ++ toString scale

    let score := [
      0, 1, 2, 3, 4, 5, 6
    ] |>.map (fun x => Note.Pitch (Pitch.Upper (Pitch.fromNat x)) (Duration.num 4)) |> Score.mk

    let inScaleScore := score |>.toInScaleScore scale |>.toScore

    IO.println $ "Scale: \n" ++ inScaleScore.toMmlCode

#eval Note.Pitch Pitch.zero (Duration.num 2).Dot |>.toMmlCode

#check [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11].map (fun x => PitchClass.fromNat x) |> Scale.fromPitchClasses
