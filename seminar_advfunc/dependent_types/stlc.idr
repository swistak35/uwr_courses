import Data.Fin
import Data.Vect


using (G : Vect n Ty)

    data Ty : Type where
        TyInt  : Ty
        TyBool : Ty
        TyFun  : Ty -> Ty -> Ty

    data HasType : Fin n -> Vect n Ty -> Ty -> Type where
        Stop : HasType FZ (t :: _) t
        Pop  : HasType i G t -> HasType (FS i) (_ :: G) t

    interpTy : Ty -> Type
    interpTy TyInt       = Int
    interpTy TyBool      = Bool
    interpTy (TyFun a r) = (interpTy a) -> (interpTy r)

    data Expr : Vect n Ty -> Ty -> Type where
        Var : HasType i G t -> Expr G t
        Val : Int -> Expr G TyInt
        Lam : Expr (a :: G) r -> Expr G (TyFun a r)
        App : Expr G (TyFun a r) -> Expr G a -> Expr G r
        Let : Expr G a -> Expr (a :: G) r -> Expr G r
        -- Fix : Expr G (TyFun (TyFun TyInt TyInt) (TyFun TyInt TyInt)) -> Expr G TyInt -> Expr G TyInt
        Op  : (interpTy a -> interpTy b -> interpTy c)
           -> Expr G a -> Expr G b -> Expr G c
        If  : Expr G TyBool
           -> Lazy (Expr G t) -> Lazy (Expr G t)
           -> Expr G t

    data Env : Vect n Ty -> Type where
        Nil  : Env Nil
        (::) : interpTy t -> Env G -> Env (t :: G)

    lookup : HasType i G t -> Env G -> interpTy t
    lookup Stop    (t :: _)  = t
    lookup (Pop i) (_ :: ts) = lookup i ts

    interp : Env G -> Expr G t -> interpTy t
    interp env (Var x)    = lookup x env
    interp env (Val c)    = c
    interp env (Lam e)    = \a => interp (a :: env) e
    interp env (App f e)  = (interp env f) (interp env e)
    interp env (Let d e)  = (\a => interp (a :: env) e) (interp env d)
    interp env (Op f l r) = f (interp env l) (interp env r)
    interp env (If c t f) = if interp env c
                            then interp env t
                            else interp env f
    -- interp env (Fix f e) = (interp env f) (\b => interp env (Fix f (Val b))) (interp env e)

    interp' : Expr Nil t -> interpTy t
    interp' = interp Nil

    plus : Expr G (TyFun TyInt (TyFun TyInt TyInt))
    plus = Lam (Lam (Op (+) (Var Stop) (Var (Pop Stop))))

    fact : Expr G (TyFun TyInt TyInt)
    fact = (Lam (If (Op (==) (Var Stop) (Val 0))
                    (Val 1)
                    (Op (*) (App fact (Op (-) (Var Stop)
                                              (Val 1)))
                            (Var Stop))))

    letTest : Expr G (TyInt)
    letTest = (Let (Val 5) (App (App plus (Var Stop)) (Val 3)))
