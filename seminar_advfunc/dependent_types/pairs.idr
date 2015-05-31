-- pairs
data Pair2 : Type -> Type -> Type where
  MkPair2 : a -> b -> Pair2 a b

Pair3 : Type -> Type -> Type
MkPair3 : a -> b -> Pair3 a b

data Fin : Nat -> Type where
  FZ : Fin (S n)
  FS : Fin n -> Fin (S n)

-- fin2Nat
fin2Nat : Fin n -> Nat
fin2Nat FZ = Z
fin2Nat (FS n) = S (fin2Nat n)

foo : Fin 6
foo = FS (FS (FS FZ))

foo2 : Fin 1
foo2 = FZ

-- nat2Fin
nat2Fin : Nat -> (n : Nat) -> Maybe (Fin n)
nat2Fin _ Z = Nothing
nat2Fin Z (S x) = Just FZ
nat2Fin x@(S x1) y@(S y1) = if x >= y then Nothing else ((nat2Fin x1 y1) >>= (\z => Just $ FS z))

foo3 : Maybe (Fin 5)
foo3 = nat2Fin 0 5

foo4 : Maybe (Fin 5)
foo4 = nat2Fin 3 5

-- fin2Nat proof
fin2NatInjective : (m : Fin k) -> (n : Fin k) -> (fin2Nat m) = (fin2Nat n) -> m = n
fin2NatInjective FZ FZ Refl = Refl
-- fin2NatInjective (FS m) FZ Refl impossible
-- fin2NatInjective FZ (FS n) Refl impossible
fin2NatInjective (FS m) (FS n) prf  =
  cong (fin2NatInjective m n (succInjective (fin2Nat m) (fin2Nat n) prf)) 

succInjective : (n : Nat) -> (m : Nat) -> S n = S m -> n = m
cong : {a : Type} -> {b : Type} -> {x : a} -> {y : a} -> {f : a -> b} -> x = y -> f x = f y

-- fin2NatInjective m n Refl = Refl



-- nat2Fin x@(S x1) y@(S y1) = if x >= y then Nothing else (case (nat2Fin x1 y1) of  --((nat2Fin x1 y) >>= (\z => Just $ FS z))
--   Just z => (Just $ FS z)
--   Nothing => Nothing)

-- nat2Fin : Nat -> (n : Nat) -> Maybe (Fin n)

