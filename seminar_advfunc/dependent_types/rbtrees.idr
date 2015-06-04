%default total

-- Dla przypomnienia warunki drzewa czerwono-czarnego:
-- 1) Korzeń jest czarny.
-- 2) Liście są czarne.
-- 3) Czerwone wierzchołki mają czarne dzieci.
-- 4) Z każdego wierzchołka każda ścieżka do liścia ma
--    taką samą liczbę czarnych wierzchołków

data Color : Type where
  R : Color
  B : Color

-- Po prostu drzewo.
-- Ten typ nam zapewnia (2) i (3).
-- Parametr n to wysokość drzewa.
-- A "Int" to wartość w danym wierzchołku.
-- W porządnej implementacji byłoby to opakowane tak,
--  żeby zamiast Int mógł być dowolny typ.
data Tree : Color -> Nat -> Type where
  E : Tree B Z
  TR : {n : Nat} ->
       Tree B n -> Int -> Tree B n ->
       Tree R n
  TB : {n : Nat} -> {c1 : Color} -> {c2 : Color} ->
       Tree c1 n -> Int -> Tree c2 n ->
       Tree B (S n)

-- Ostateczny typ naszego drzewa czerwono-czarnego,
--  w porządnej implementacji to byłby typ dostępny przez API
--  (reszta byłaby niedostępna).
-- Ten typ nam daje (1).
data RBT : Type where
  Root : {n : Nat} -> Tree B n -> RBT

-- Poprawne drzewo, ale bez ekspozycji koloru w typie,
--  więc korzystając z tego typu nie można
--  np. sprawdzić czy korzeń na pewno jest czarny.
-- tl;dr - uboższa wersja Tree
data HiddenTree : Nat -> Type where
  HR : {m : Nat} ->
       Tree R m ->
       HiddenTree m
  HB : {m : Nat} ->
       Tree B (S m) ->
       HiddenTree (S m)

incr : Color -> Nat -> Nat
incr B = S
incr R = id

-- Drzewo które zachowuje tylko poprawną wartość wysokości drzewa,
--  ale nie pilnuje kolorów (3)
data AlmostTree : Nat -> Type where
  AT : {n : Nat} -> {c1 : Color} -> {c2 : Color} ->
       (c : Color) -> Tree c1 n -> Int -> Tree c2 n ->
       AlmostTree (incr c n)

balanceLB : {n : Nat} -> {c : Color} ->
            AlmostTree n -> Int -> Tree c n ->
            HiddenTree (S n)
balanceLB (AT R (TR a x b) y c) z d = HR (TR (TB a x b) y (TB c z d))
balanceLB (AT R a x (TR b y c)) z d = HR (TR (TB a x b) y (TB c z d))
balanceLB (AT B a x b) y r = HB (TB (TB a x b) y r)
balanceLB (AT R E x E) y r = HB (TB (TR E x E) y r)
balanceLB (AT R (TB a w b) x (TB c y d)) z e = HB (TB (TR (TB a w b) x (TB c y d)) z e)

balanceLR : {n : Nat} -> {c : Color} ->
            HiddenTree n -> Int -> Tree c n ->
            AlmostTree n
balanceLR (HR l) x r = AT R l x r
balanceLR (HB l) x r = AT R l x r

balanceRR : {n : Nat} -> {c : Color} ->
            Tree c n -> Int -> HiddenTree n ->
            AlmostTree n
balanceRR l x (HR r) = AT R l x r
balanceRR l x (HB r) = AT R l x r

balanceRB : {n : Nat} -> {c : Color} ->
            Tree c n -> Int -> AlmostTree n ->
            HiddenTree (S n)
balanceRB a x (AT R (TR b y c)  z d) = HR (TR (TB a x b) y (TB c z d))
balanceRB a x (AT R b y (TR c z d)) = HR (TR (TB a x b) y (TB c z d))
balanceRB a x (AT R E y E) = HB (TB a x (TR E y E))
balanceRB a x (AT R (TB l y r) x' (TB l' y' r')) = HB (TB a x (TR (TB l y r) x' (TB l' y' r')))
balanceRB a x (AT B l y r) = HB (TB a x (TB l y r))

blacken : {n : Nat} -> HiddenTree n -> RBT
blacken (HR (TR l x r)) = Root (TB l x r)
blacken (HB (TB l x r)) = Root (TB l x r)

-- Zwolnienie warunków - dajemy drzewo które spełnia (3),
--  a dostajemy typ który o tym nie pamięta.
forget : {n : Nat} -> HiddenTree n -> AlmostTree n
forget (HR (TR l x r)) = AT R l x r
forget (HB (TB l x r)) = AT B l x r

mutual
  insBlack : {n : Nat} -> (t : Tree B n) -> (x : Int) -> HiddenTree n
  insBlack E x = HR (TR E x E)
  insBlack (TB l y r) x = case (compare x y) of
    LT => balanceLB (ins l x) y r
    GT => balanceRB l y (ins r x)
    EQ => HB (TB l x r)

  ins : {n : Nat} -> {c : Color} -> (t : Tree c n) -> (x : Int) -> AlmostTree n
  ins (TR l y r) x = case (compare x y) of
    LT => balanceLR (insBlack l x) y r
    GT => balanceRR l y (insBlack r x)
    EQ => AT R l x r
  ins (TB l y r) x = forget (insBlack (TB l y r) x)
  ins E x          = forget (insBlack E x)

empty : RBT
empty = Root E

insert : RBT -> Int -> RBT
insert (Root t) x = blacken (insBlack t x)
