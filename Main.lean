import MusicCompositionLang

open MusicCompositionLang

def main : IO Unit := do
    let scale := [-3, -3, 0, 2, 4, 5, 7].map (fun x => Pitch.fromInt x)
      |> Scale.fromPitches
    IO.println $ "Scale: " ++ reprStr scale

    let score := [
      0, 1, 2, 3, 4, 5, 6
    ] |>.map (fun x => Note.Pitch (Pitch.fromNat x) (Duration.num 4))

    let score := InScaleScore.mk score scale |>.toScore

    IO.println $ score |> reprStr

    let scoreUpperOctave := score.transpose 12

    IO.println $ scoreUpperOctave |> reprStr

    let scoreLowerOctave := score |>.transpose (-12)

    IO.println $ scoreLowerOctave |> reprStr

    let scoreLowerOctaveDotted: Score := score |>.transpose (-12) |>.map (fun note => match note with
      | Note.Pitch pitch duration => Note.Pitch pitch duration.addDot
      | _ => note
    )

    IO.println $ scoreLowerOctaveDotted |> reprStr

    IO.println $ scoreLowerOctaveDotted |>.toMmlCode

    IO.println $ scoreLowerOctave |>.toMmlCode

    IO.println $ score|>.toMmlCode

    IO.println $ scoreUpperOctave |>.toMmlCode

