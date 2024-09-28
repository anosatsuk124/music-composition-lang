namespace MusicCompositionLang

inductive Pitch where
  | zero : Pitch
  | one : Pitch
  | two : Pitch
  | three : Pitch
  | four : Pitch
  | five : Pitch
  | six : Pitch
  | seven : Pitch
  | eight : Pitch
  | nine : Pitch
  | ten : Pitch
  | eleven : Pitch
  | Upper : Pitch -> Pitch
  | Lower : Pitch -> Pitch
deriving Repr, Inhabited

abbrev InScalePitch := Pitch

def Pitch.toInt : Pitch -> Int
  | Pitch.zero => 0
  | Pitch.one => 1
  | Pitch.two => 2
  | Pitch.three => 3
  | Pitch.four => 4
  | Pitch.five => 5
  | Pitch.six => 6
  | Pitch.seven => 7
  | Pitch.eight => 8
  | Pitch.nine => 9
  | Pitch.ten => 10
  | Pitch.eleven => 11
  | Pitch.Upper p => Pitch.toInt p + 12
  | Pitch.Lower p => Pitch.toInt p - 12

def Pitch.fromNat : Nat -> Pitch
  | 0 => Pitch.zero
  | 1 => Pitch.one
  | 2 => Pitch.two
  | 3 => Pitch.three
  | 4 => Pitch.four
  | 5 => Pitch.five
  | 6 => Pitch.six
  | 7 => Pitch.seven
  | 8 => Pitch.eight
  | 9 => Pitch.nine
  | 10 => Pitch.ten
  | 11 => Pitch.eleven
  | n + 12 => Pitch.Upper (fromNat n)

def Pitch.fromInt : Int -> Pitch
  | Int.ofNat n => fromNat n
  | Int.negSucc n =>
    let rec fromNeg: Nat -> Pitch := fun n => match n with
      | 0 => Pitch.Lower Pitch.eleven
      | 1 => Pitch.Lower Pitch.ten
      | 2 => Pitch.Lower Pitch.nine
      | 3 => Pitch.Lower Pitch.eight
      | 4 => Pitch.Lower Pitch.seven
      | 5 => Pitch.Lower Pitch.six
      | 6 => Pitch.Lower Pitch.five
      | 7 => Pitch.Lower Pitch.four
      | 8 => Pitch.Lower Pitch.three
      | 9 => Pitch.Lower Pitch.two
      | 10 => Pitch.Lower Pitch.one
      | 11 => Pitch.Lower Pitch.zero
      | n + 12 => Pitch.Lower (fromNeg n)
    fromNeg n

def Pitch.succ : Pitch -> Pitch
  | Pitch.zero => Pitch.one
  | Pitch.one => Pitch.two
  | Pitch.two => Pitch.three
  | Pitch.three => Pitch.four
  | Pitch.four => Pitch.five
  | Pitch.five => Pitch.six
  | Pitch.six => Pitch.seven
  | Pitch.seven => Pitch.eight
  | Pitch.eight => Pitch.nine
  | Pitch.nine => Pitch.ten
  | Pitch.ten => Pitch.eleven
  | Pitch.eleven => Pitch.Upper Pitch.zero
  | Pitch.Upper p => Pitch.Upper (Pitch.succ p)
  | Pitch.Lower p => 
    match p with
      | Pitch.eleven => Pitch.zero
      | _ => Pitch.Lower (Pitch.succ p)

def Pitch.pred : Pitch -> Pitch
  | Pitch.zero => Pitch.Lower Pitch.eleven
  | Pitch.one => Pitch.zero
  | Pitch.two => Pitch.one
  | Pitch.three => Pitch.two
  | Pitch.four => Pitch.three
  | Pitch.five => Pitch.four
  | Pitch.six => Pitch.five
  | Pitch.seven => Pitch.six
  | Pitch.eight => Pitch.seven
  | Pitch.nine => Pitch.eight
  | Pitch.ten => Pitch.nine
  | Pitch.eleven => Pitch.ten
  | Pitch.Lower p =>  Pitch.Lower (Pitch.pred p)
  | Pitch.Upper p =>
    match p with
      | Pitch.zero => Pitch.eleven
      | _ => Pitch.Upper (Pitch.pred p)

instance : LT Pitch where
  lt := fun p1 p2 => (Pitch.toInt p1) < (Pitch.toInt p2)

instance: LE Pitch where
  le := fun p1 p2 => (Pitch.toInt p1) <= (Pitch.toInt p2)

def Pitch.add : Pitch -> Nat -> Pitch := fun p n =>
  let rec addAux : Pitch -> Nat -> Pitch
    | p, 0 => p
    | p, n + 1 => addAux p.succ n
  addAux p n

def Pitch.sub : Pitch -> Nat -> Pitch := fun p n =>
  let rec subAux : Pitch -> Nat -> Pitch
    | p, 0 => p
    | p, n + 1 => subAux p.pred n
  subAux p n

instance : HAdd Pitch Nat Pitch where
  hAdd := Pitch.add

instance : HSub Pitch Nat Pitch where
  hSub := Pitch.sub

def Pitch.upOctave : Pitch -> Nat -> Pitch := fun p n =>
  let rec upAux : Pitch -> Nat -> Pitch
    | p, 0 => p
    | p, n + 1 => upAux (p + 12) n
  upAux p n

def Pitch.downOctave : Pitch -> Nat -> Pitch := fun p n =>
  let rec downAux : Pitch -> Nat -> Pitch
    | p, 0 => p
    | p, n + 1 => downAux (p - 12) n
  downAux p n

def Pitch.getOctave : Pitch -> Int := fun p => (Pitch.toInt p) / 12

