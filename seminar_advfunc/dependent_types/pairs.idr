data Pair2 : Type -> Type -> Type where
  MkPair2 : a -> b -> Pair2 a b

Pair3 : Type -> Type -> Type
MkPair3 : a -> b -> Pair3 a b

data Fin : Nat -> Type where
  FZ : Fin (S n)
  FS : Fin n -> Fin (S n)

fin2Nat : Fin n -> Nat
fin2Nat FZ = Z
fin2Nat (FS n) = S (fin2Nat n)

foo : Fin 4
foo = FS (FS (FS FZ))

foo2 : Fin 1
foo2 = FZ

nat2Fin : Nat -> (n : Nat) -> Maybe (Fin n)
nat2Fin _ Z = Nothing
nat2Fin Z (S x) = Just FZ
nat2Fin (S x1) y@(S y1) = if x1 >= y1 then Nothing else ((nat2Fin x1 y) >>= (\z => Just $ FS z))

-- nat2Fin : Nat -> (n : Nat) -> Maybe (Fin n)

foo3 : Maybe (Fin 5)
foo3 = nat2Fin 0 5
