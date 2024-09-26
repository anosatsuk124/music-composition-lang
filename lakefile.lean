import Lake
open Lake DSL

require mathlib from git
  "https://github.com/leanprover-community/mathlib4" @ "v4.11.0"

require batteries from git
  "https://github.com/leanprover-community/batteries" @ "v4.11.0"

package "music-composition-lang" where
  -- add package configuration options here

lean_lib «MusicCompositionLang» where
  roots := #[`Lib, `MusicCompositionLang]
  -- add library configuration options here

@[default_target]
lean_exe "music-composition-lang" where
  root := `Main
