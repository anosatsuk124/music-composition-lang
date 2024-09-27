import MusicCompositionLang

open MusicCompositionLang

def main : IO Unit := do
    IO.println $ Score.toMmlCode $ [Note.Pitch $ Pitch.Upper Pitch.zero, Note.InScale
      ([0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11].map (fun x => PitchClass.fromNat x) |> Scale.fromPitchClasses) Pitch.zero]

#eval Note.InScale ([0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11].map (fun x => PitchClass.fromNat x) |> Scale.fromPitchClasses) Pitch.zero |>.toMmlCode

#check [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11].map (fun x => PitchClass.fromNat x) |> Scale.fromPitchClasses
