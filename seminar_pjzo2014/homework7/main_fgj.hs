-- Imports
import qualified Data.Map.Strict as M
import qualified Control.Monad as C
import qualified Data.Either as E
import Data.List((\\))
import qualified Data.List as L
import Data.Maybe
import Debug.Trace
import qualified FeatherweightJava as FJ


-- Data types
type KlassName = String
type MethodName = String
type FieldName = String
type ObjectName = String

data KlassType = SimpleKlass KlassName | ComplexKlass KlassGeneric deriving (Eq, Show)

data KlassGeneric = KlassGeneric KlassName [KlassType] deriving (Eq, Show)

data Generic = Generic
        { gName :: KlassName
        , gParent :: KlassGeneric
        , gFields :: [(FieldName, KlassType)]
        , gParams :: [(KlassName, KlassGeneric)]
        , gMethods :: M.Map MethodName Method
} deriving (Eq, Show)

data Method = Method
        { mName :: MethodName
        , mArgs :: [(ObjectName, KlassType)]
        , mExpr :: Expr
        , mRetType :: KlassType
        , mCast :: [(KlassName, KlassGeneric)]
} deriving (Eq, Show)

data Expr = Arg ObjectName
        | FieldCall Expr FieldName
        | MethodCall Expr MethodName [Expr] [KlassType]
        | NewObject KlassGeneric [Expr]
        | TypeCast Expr KlassGeneric deriving (Eq, Show)

data TypeRelation = Subtype | Supertype | Equal | None deriving (Eq, Show)

type KlassTable = M.Map KlassName Generic
type Program = (KlassTable, Expr)
type Context = M.Map KlassName KlassGeneric
type Environment = M.Map ObjectName KlassType

mk_SimpleClass :: KlassName -> KlassType
mk_SimpleClass name = SimpleKlass name

mk_ComplexClass :: KlassGeneric -> KlassType
mk_ComplexClass gen = ComplexKlass gen

lookupe :: (Eq a) => a -> [(a,b)] -> b
lookupe x xs = case (lookup x xs) of
        Just y  -> y
        Nothing -> error "lookupe fail."

get_generic :: KlassTable -> KlassName -> Generic
get_generic kt kName = kt M.! kName

klass_type_substitute :: M.Map KlassName KlassType -> KlassType -> KlassType
klass_type_substitute mapper kType = case kType of
        ComplexKlass (KlassGeneric kName kts)   -> ComplexKlass (KlassGeneric kName (map (klass_type_substitute mapper) kts))
        SimpleKlass kName                       -> (case (M.lookup kName mapper) of
                Just kType'     -> kType'
                Nothing         -> kType)

klass_generic_substitute :: M.Map KlassName KlassType -> KlassGeneric -> KlassGeneric
klass_generic_substitute mapper kGeneric = kGeneric'
        where   ComplexKlass kGeneric' = klass_type_substitute mapper (ComplexKlass kGeneric)

expr_substitute :: M.Map KlassName KlassType -> Expr -> Expr
expr_substitute mapper expr = case expr of
        Arg x                           -> expr
        FieldCall expr fName            -> FieldCall (expr_substitute mapper expr) fName
        MethodCall expr mName es kts    -> MethodCall (expr_substitute mapper expr) mName (map (expr_substitute mapper) es) (map (klass_type_substitute mapper) kts)
        NewObject kGeneric es           -> NewObject (klass_generic_substitute mapper kGeneric) (map (expr_substitute mapper) es)
        TypeCast es kGeneric            -> TypeCast (expr_substitute mapper expr) (klass_generic_substitute mapper kGeneric)

mk_type_mapper :: [KlassName] -> [KlassType] -> M.Map KlassName KlassType
mk_type_mapper type_variables kts = M.fromList (zip type_variables kts)


fields :: KlassTable -> KlassGeneric -> [(FieldName, KlassType)]
fields kt (KlassGeneric "Object" _) = []
fields kt (KlassGeneric gName kts) = parent_fields ++ generic_fields
        where   (Generic _ gParent gFields gParams _) = get_generic kt gName
                type_mapper = mk_type_mapper (map fst gParams) kts
                parent_fields = fields kt (klass_generic_substitute type_mapper gParent)
                kts' = map (klass_type_substitute type_mapper) kts
                generic_fields = zip (map fst gFields) kts'


mtype :: KlassTable -> KlassGeneric -> MethodName -> ([(KlassName, KlassGeneric, KlassType)], KlassType)
mtype kt kGeneric mName = case (M.lookup mName gMethods) of
        Just m          -> (mArgs'', mRetType')
                where   (Method _ mArgs _ mRetType mCast) = m
                        mRetType' = klass_type_substitute type_mapper mRetType
                        mArgs' = uncurry zip3 (unzip mCast) (map snd mArgs)
                        mArgs'' = map (\(x,y,z) -> (x, klass_generic_substitute type_mapper y, klass_type_substitute type_mapper z)) mArgs'
        Nothing         -> mtype kt kGeneric' mName
        where   (KlassGeneric gName kts) = kGeneric
                (Generic _ gParent _ gParams gMethods) = get_generic kt gName
                type_mapper = mk_type_mapper (map fst gParams) kts
                kGeneric' = klass_generic_substitute type_mapper gParent

mbody :: KlassTable -> KlassGeneric -> MethodName -> [KlassType] -> ([KlassName], Expr)
mbody kt kGeneric mName mValues = case (M.lookup mName gMethods) of
        Just m          -> (mVars, mExpr')
                where   (Method _ mArgs mExpr mRetType mCast) = m
                        mVars = map fst mArgs
                        type_mapper' = mk_type_mapper (map fst mCast) mValues
                        new_type_mapper = M.union type_mapper type_mapper'
                        mExpr' = expr_substitute new_type_mapper mExpr
        Nothing         -> mbody kt kGeneric' mName mValues
        where   (KlassGeneric gName kts) = kGeneric
                (Generic _ gParent _ gParams gMethods) = get_generic kt gName
                type_mapper = mk_type_mapper (map fst gParams) kts
                kGeneric' = klass_generic_substitute type_mapper gParent

bound :: Context -> KlassType -> KlassGeneric
bound ctx kType = case kType of
        SimpleKlass name        -> ctx M.! name
        ComplexKlass kGeneric   -> kGeneric

isSimpleKlass :: KlassType -> Bool
isSimpleKlass kType = case kType of
        SimpleKlass _   -> True
        _               -> False

isSubclassOf :: KlassTable -> KlassName -> KlassName -> Bool
isSubclassOf kt k1 k2
        | k1 == k2              = True
        | k2 == pName           = True
        | k1 == "Object"        = False
        | otherwise             = isSubclassOf kt pName k2
        where   (Generic _ (KlassGeneric pName _) _ _ _) = get_generic kt k1



isSubtypeOf :: KlassTable -> Context -> KlassType -> KlassType -> Bool
isSubtypeOf kt ctx k1 k2
        | k1 == k2                                      = True
        | k1 == ComplexKlass (KlassGeneric "Object" []) = False
        | otherwise                                     = case k1 of
                SimpleKlass k1Name                      -> (kc == k2) || (isSubtypeOf kt ctx kc k2)
                        where   kc = ComplexKlass (ctx M.! k1Name)
                ComplexKlass (KlassGeneric k1Name kts)  -> (kp == k2) || (isSubtypeOf kt ctx kp k2)
                        where   (Generic _ kGenericParent _ gParams _) = get_generic kt k1Name
                                type_mapper = mk_type_mapper (map fst gParams) kts
                                kp = ComplexKlass (klass_generic_substitute type_mapper kGenericParent)

isWellFormed :: KlassTable -> Context -> KlassType -> Bool
isWellFormed kt ctx (ComplexKlass (KlassGeneric "Object" _)) = True
isWellFormed kt ctx (SimpleKlass kName) = kName `elem` (M.keys ctx)
isWellFormed kt ctx (ComplexKlass (KlassGeneric gName kts)) = params_ok && subtypes_ok
        where   (Generic _ _ _ gParams _) = get_generic kt gName
                params_ok = and$ map (isWellFormed kt ctx) kts
                bounds = map snd gParams
                type_mapper = mk_type_mapper (map fst gParams) kts
                bounds' = map (ComplexKlass . (klass_generic_substitute type_mapper)) bounds
                subtypes_ok = and$ map (uncurry$ isSubtypeOf kt ctx) $ zip kts bounds'

check_class :: KlassTable -> Context -> KlassGeneric -> Bool
check_class kt ctx kGeneric = True

set_equal :: (Eq a) => [a] -> [a] -> Bool
set_equal xs ys = (xs' \\ ys' == []) && (ys' \\ xs' == [])
        where   xs' = L.nub xs
                ys' = L.nub ys

free_variables :: KlassType -> [KlassName]
free_variables (SimpleKlass kName) = [kName]
free_variables (ComplexKlass (KlassGeneric _ kts)) = concatMap free_variables kts

downcast :: KlassTable -> KlassName -> KlassName -> Bool
downcast kt k1 k2
        | k1 == "Object"        = False
        | k2 == pName           = set_equal type_vars free_vars
        | otherwise             = downcast kt pName k2
        where   (Generic _ gParent _ gParams _) = get_generic kt k1
                (KlassGeneric pName kts) = gParent
                type_vars = map fst gParams
                free_vars = concatMap free_variables kts

fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a

snd3 :: (a,b,c) -> b
snd3 (_,b,_) = b

trd3 :: (a,b,c) -> c
trd3 (_,_,c) = c

typeof :: KlassTable -> Context -> Environment -> Expr -> KlassType
typeof kt ctx env (Arg x) = env M.! x

typeof kt ctx env (FieldCall expr0 fName) = lookupe fName expr0_fields
        where   expr0_type = typeof kt ctx env expr0
                expr0_fields = fields kt (bound ctx expr0_type)

typeof kt ctx env expr@(NewObject kGeneric es) = if and (klass_ok:subtypes)
        then ComplexKlass kGeneric else error ("Error when typechecking `"++(show expr)++"`.\n")
        where   klass_ok = check_class kt ctx kGeneric
                field_types = map snd $ fields kt kGeneric
                arg_types = map (typeof kt ctx env) es
                subtypes = map (uncurry $ isSubtypeOf kt ctx) $ zip arg_types field_types

typeof kt ctx env expr@(MethodCall expr0 mName es kts) = if everything_ok
        then ret_type' else error ("Error when typechecking `"++(show expr)++"`.\n")
        where   expr0_type = typeof kt ctx env expr0
                es_types = map (typeof kt ctx env) es
                (arg_types, ret_type) = mtype kt (bound ctx expr0_type) mName
                type_mapper = mk_type_mapper (map fst3 arg_types) kts
                type_vars_ok = and$ map (isWellFormed kt ctx) kts
                vars_subtypes_ok = and$ map (uncurry $ isSubtypeOf kt ctx) (zip kts (map (ComplexKlass . (klass_generic_substitute type_mapper) . snd3) arg_types))
                val_subtypes_ok = and$ map (uncurry $ isSubtypeOf kt ctx) (zip es_types (map (klass_type_substitute type_mapper . trd3) arg_types))
                everything_ok = type_vars_ok && vars_subtypes_ok && val_subtypes_ok
                ret_type' = klass_type_substitute type_mapper ret_type

typeof kt ctx env expr@(TypeCast expr0 kGeneric)
        | isSubtypeOf kt ctx bounded_expr0_type ret_type                = ret_type
        | isSubtypeOf kt ctx ret_type bounded_expr0_type && dcast_conds = ret_type
        | scast_conds                                                   = ret_type
        | otherwise                                                     = error "Impossible!"
        where   expr0_type = typeof kt ctx env expr0
                bounded_expr0_kgen = bound ctx expr0_type
                bounded_expr0_type = (ComplexKlass bounded_expr0_kgen)
                ret_type = (ComplexKlass kGeneric)
                (KlassGeneric kGenericName _) = kGeneric
                (KlassGeneric bounded_name bounded_vars) = bounded_expr0_kgen
                ret_type_ok = isWellFormed kt ctx ret_type
                dcast_conds = (downcast kt kGenericName bounded_name) && ret_type_ok
                scast_conds = ret_type_ok && (not (isSubclassOf kt kGenericName bounded_name)) && (not (isSubclassOf kt bounded_name kGenericName))

type_erasure :: Context -> KlassType -> KlassName
type_erasure ctx kType = name
        where   (KlassGeneric name _) = bound ctx kType

mk_context :: [(KlassName, KlassGeneric)] -> M.Map KlassName KlassGeneric
mk_context = M.fromList

join_context :: M.Map KlassName KlassGeneric -> M.Map KlassName KlassGeneric -> M.Map KlassName KlassGeneric
join_context c1 c2 = mk_context $ (M.toList c1) ++ (M.toList c2)

fieldsmax :: KlassTable -> KlassName -> M.Map FieldName KlassName
fieldsmax kt kName = M.fromList (fieldsmax' kt kName)
        where   fieldsmax' kt "Object" = []
                fieldsmax' kt kName = parent_fields ++ klass_fields
                        where   (Generic _ (KlassGeneric pName _) gFields gParams _) = get_generic kt kName
                                parent_fields = fieldsmax' kt pName
                                ctx = mk_context gParams
                                klass_fields = zip (map (type_erasure ctx . snd) gFields) (map fst gFields)

is_defined :: KlassTable -> MethodName -> KlassName -> Bool
is_defined kt mName kName = mName `elem` (M.keys gMethods)
        where   (Generic _ _ _ _ gMethods) = get_generic kt kName

get_method :: KlassTable -> KlassName -> MethodName -> Method
get_method kt kName mName = gMethods M.! mName
        where   (Generic _ _ _ _ gMethods) = get_generic kt kName

mtypemax :: KlassTable -> MethodName -> KlassName -> ([FJ.KlassName], FJ.KlassName)
mtypemax kt mName kName = if is_defined kt mName pName then mtypemax kt mName pName else (mArgs', mRetType')
        where   (Generic gName gParent _ gParams _) = get_generic kt kName
                (Method mName mArgs _ mRetType mCast) = get_method kt kName mName
                (KlassGeneric pName _) = gParent
                ctx = mk_context (gParams ++ mCast)
                mRetType' = type_erasure ctx mRetType
                mArgs' = map (type_erasure ctx) (map snd mArgs)

erasure :: KlassTable -> Context -> Environment -> Expr -> FJ.Expr
erasure kt ctx env (Arg x) = FJ.Arg x

erasure kt ctx env expr@(FieldCall expr0 fName)
        | field_type == expr_type       = expr'
        | otherwise                     = FJ.TypeCast expr' expr_type
        where   expr_type = type_erasure ctx (typeof kt ctx env expr)
                expr0_type = type_erasure ctx (typeof kt ctx env expr0)
                field_type = (fieldsmax kt expr0_type) M.! fName
                expr0' = erasure kt ctx env expr0
                expr' = FJ.FieldCall expr0' fName

erasure kt ctx env expr@(MethodCall expr0 mName es kts)
        | return_type == expr_type      = expr'
        | otherwise                     = FJ.TypeCast expr' expr_type
        where   expr_type = type_erasure ctx (typeof kt ctx env expr)
                expr0_type = type_erasure ctx (typeof kt ctx env expr0)
                expr0' = erasure kt ctx env expr0
                es' = map (erasure kt ctx env) es
                (_,return_type) = mtypemax kt mName expr0_type
                expr' = FJ.MethodCall expr0' mName es'

erasure kt ctx env expr@(NewObject kGeneric es) = FJ.NewObject kType' es'
        where   es' = map (erasure kt ctx env) es
                kType' = type_erasure ctx (ComplexKlass kGeneric)

erasure kt ctx env expr@(TypeCast expr0 kGeneric) = FJ.TypeCast expr0' kType'
        where   expr0' = erasure kt ctx env expr0
                kType' = type_erasure ctx (ComplexKlass kGeneric)

klass_expr_substitute :: M.Map ObjectName FJ.Expr -> FJ.Expr -> FJ.Expr
klass_expr_substitute mapper expr@(FJ.Arg x) = if x `elem` (M.keys mapper) then mapper M.! x else expr
klass_expr_substitute mapper (FJ.FieldCall expr0 fName) = FJ.FieldCall (klass_expr_substitute mapper expr0) fName
klass_expr_substitute mapper (FJ.MethodCall expr0 mName es) = FJ.MethodCall expr0' mName es'
        where   expr0' = klass_expr_substitute mapper expr0
                es' = map (klass_expr_substitute mapper) es
klass_expr_substitute mapper (FJ.NewObject kName es) = FJ.NewObject kName (map (klass_expr_substitute mapper) es)
klass_expr_substitute mapper (FJ.TypeCast expr0 kType) = FJ.TypeCast (klass_expr_substitute mapper expr0) kType

compile_method :: KlassTable -> Generic -> Context -> Method -> FJ.Method
compile_method kt gen ctx m@(Method mName mArgs mExpr mRetType mCast) = m'
        where   (Generic gName _ _ _ _) = gen
                (arg_types, mRetType') = mtypemax kt mName gName
                new_ctx = join_context ctx (mk_context mCast)
                env = M.fromList $ ("this", ComplexKlass (KlassGeneric gName (map mk_SimpleClass (M.keys ctx)))):mArgs
                var_map_fun (d,(x,t)) = if d == t then (x, FJ.Arg x) else (x, FJ.TypeCast (FJ.Arg x) t)
                var_mapper = M.fromList $ map var_map_fun (zip arg_types (map (\(x,t) -> (x, type_erasure new_ctx t)) mArgs))
                mExpr' = klass_expr_substitute var_mapper (erasure kt ctx env mExpr)
                mArgs' = zip (map fst mArgs) arg_types
                m' = FJ.Method mName mArgs' mExpr' mRetType'

compile_class :: KlassTable -> Generic -> FJ.Klass
compile_class kt gen@(Generic gName (KlassGeneric pName kts) gFields gParams gMethods) = klass
        where   ctx = mk_context gParams
                kFields = map (\(n,k) -> (n, type_erasure ctx k)) gFields
                kMethods = M.map (compile_method kt gen ctx) gMethods
                klass = FJ.Klass gName pName kFields kMethods

--example_kt_pair = M.fromList [("A", Klass "A" "Object" [] (M.fromList [])), ("B", Klass "B" "Object" [] (M.fromList [])),                        ("Pair", Klass "Pair" "Object" [("fst", "Object"), ("snd", "Object")] (M.fromList [("setfst", Method "setfst" [("newfst", "Object")] (NewObject "Pair" [Arg "newfst", FieldCall (Arg "this") "snd"]) "Pair")]))                ]
--example1 = (example_kt_pair, expr)
--        where   expr = MethodCall (NewObject "Pair" [NewObject "A" [], NewObject "B" []]) "setfst" [NewObject "B" []]

object_generic = KlassGeneric "Object" []
example_kt_pair = M.fromList [a_generic, b_generic, pair_generic]
        where   a_generic = ("A", Generic "A" object_generic [] [] M.empty)
                b_generic = ("B", Generic "B" object_generic [] [] M.empty)
                pair_fields = [("fst", SimpleKlass "X"), ("snd", SimpleKlass "Y")]
                pair_params = [("X", object_generic), ("Y", object_generic)]
                setfst_args = [("newfst", SimpleKlass "Z")]
                setfst_expr = NewObject (KlassGeneric "Pair" [SimpleKlass "Z", SimpleKlass "Y"]) [Arg "newfst", FieldCall (Arg "this") "snd"]
                setfst_ret_type = ComplexKlass (KlassGeneric "Pair" [SimpleKlass "Z", SimpleKlass "Y"])
                setfst_cast = [("Z", object_generic)]
                setfst_method = Method "setfst" setfst_args setfst_expr setfst_ret_type setfst_cast
                pair_methods = M.fromList [("setfst", setfst_method)]
                pair_generic = ("Pair", Generic "Pair" object_generic pair_fields pair_params pair_methods)

example1 = (example_kt_pair, expr)
        where   expr = (NewObject (KlassGeneric "Pair" [ComplexKlass (KlassGeneric "A" []), ComplexKlass (KlassGeneric "B" [])]) [NewObject (KlassGeneric "A" []) [],NewObject (KlassGeneric "B" []) []])



compile :: Program -> FJ.Program
compile prog@(kt, expr) = (kt', expr')
        where   kt' = M.map (compile_class kt) kt
                expr' = erasure kt M.empty M.empty expr

main = do
        putStrLn "It works"
        --res <- compile example1
        --putStrLn $ (show res)
        --if res
        --then putStrLn $ show $ evaluate M.empty example1
        --else return$ ()







---- Utils
--getRight :: Either a b -> b
--getRight (Right x) = x
--getRight _ = error "getRight: unexpected Left"

---- Typechecker
--inh_tree :: KlassTable -> KlassName -> Either String [KlassName]
--inh_tree kt name = C.liftM reverse $ inh_tree' kt name []

--inh_tree' :: KlassTable -> KlassName -> [KlassName] -> Either String [KlassName]
--inh_tree' kt "Object" acc = Right ("Object":acc)
--inh_tree' kt name acc = do
--        pName <- C.liftM kParent $ klassLookup kt name
--        if name `elem` acc
--        then Left ("Loop in inheritance tree of "++name)
--        else inh_tree' kt pName (name:acc)

--type_relation :: KlassTable -> KlassName -> KlassName -> Either String TypeRelation
--type_relation kt x y = do
--        xtree <- inh_tree kt x
--        ytree <- inh_tree kt y
--        return $ case (x `elem` ytree, y `elem` xtree) of
--                (True, True)    -> Equal
--                (True, False)   -> Supertype
--                (False, True)   -> Subtype
--                (False, False)  -> None

--isSubtypeOf :: KlassTable -> KlassName -> KlassName -> Either String Bool
--isSubtypeOf kt x y = C.liftM (\x -> x `elem` [Equal, Subtype]) (type_relation kt x y)

--fields' :: KlassTable -> KlassName -> ((FieldName, ObjectName) -> a) -> Either String [a]
--fields' kt "Object" f = Right []
--fields' kt name f = do
--        Klass _ pname fs _ <- klassLookup kt name
--        pRes <- fields' kt pname f
--        return $ pRes ++ (map f fs)

--fields :: KlassTable -> KlassName -> Either String [KlassName]
--fields kt name = fields' kt name snd

--fieldNames :: KlassTable -> KlassName -> Either String [FieldName]
--fieldNames kt name = fields' kt name fst

--mtype :: KlassTable -> KlassName -> MethodName -> Either String (Maybe ([KlassName], KlassName))
--mtype kt "Object" _ = Right Nothing
--mtype kt kName name = do
--        klass <- klassLookup kt kName
--        case (M.lookup name $ kMethods klass) of
--                Just method     -> return$ Just (map snd (mArgs method), mRetType method)
--                Nothing         -> mtype kt (kParent klass) name

--methodLookup' :: KlassTable -> KlassName -> MethodName -> Either String (Maybe Method)
--methodLookup' kt "Object" _ = Right Nothing
--methodLookup' kt kName name = do
--        klass <- klassLookup kt kName
--        case (M.lookup name $ kMethods klass) of
--                Just method     -> return$ Just method
--                Nothing         -> methodLookup' kt (kParent klass) name


--klassLookup :: KlassTable -> KlassName -> Either String Klass
--klassLookup kt name = case (M.lookup name kt) of
--        Just klass      -> Right klass
--        Nothing         -> Left ("There is no `"++name++"` in Class Table.")

--fieldLookup :: KlassTable -> KlassName -> FieldName -> Either String KlassName
--fieldLookup kt "Object" name = Left ("There is no field `"++name++"`.")
--fieldLookup kt kName name = (klassLookup kt kName) >>=
--        (\(Klass _ pname fs _) -> case (lookup name fs) of
--                Just kName      -> Right kName
--                Nothing         -> fieldLookup kt pname name)

--methodLookup :: Klass -> MethodName -> Either String Method
--methodLookup klass name = case (M.lookup name (kMethods klass)) of
--        Nothing         -> Left ("Method `"++(kName klass)++"`.`"++name++"`")
--        Just method     -> Right method

--compare_types :: KlassTable -> [KlassName] -> [KlassName] -> Either String Bool
--compare_types kt xs ys = Right $ ((length xs) == (length ys)) && (and $ E.rights $ map (uncurry $ isSubtypeOf kt) $ zip xs ys)

--equal_types :: [KlassName] -> [KlassName] -> Bool
--equal_types xs ys = ((length xs) == (length ys)) && (all (uncurry (==)) $ zip xs ys)

--checkelist :: M.Map ObjectName KlassName -> KlassTable -> [Expr] -> Either String [KlassName]
--checkelist args kt es = mapM (curry (checketype args) kt) es

--checketype :: M.Map ObjectName KlassName -> Program -> Either String KlassName
--checketype args (kt, e@(TypeCast expr name)) = (checketype args (kt,expr)) >> (return name)

--checketype args (kt, e@(NewObject name es)) = do -- T-New
--        fst <- fields kt name
--        est <- checkelist args kt es
--        compRes <- compare_types kt est fst
--        if compRes
--        then Right name
--        else Left ("Argument types are not ok in constructor of `"++name++"`.")

--checketype args (kt, e@(MethodCall expr name es)) = do -- T-Invk
--        expr_type <- checketype args (kt, expr)
--        est <- checkelist args kt es
--        mtypes <- mtype kt expr_type name
--        maybe method_not_found (\(argTypes, retType) ->
--                (compare_types kt est argTypes) >>= (\res ->
--                        if res then Right retType else wrong_arguments)) mtypes
--        where   method_not_found = Left ("Method `"++name++"` not found.")
--                wrong_arguments = Left ("Argument's types are not ok in `"++name++"` call.")

--checketype args (kt, e@(FieldCall expr name)) = (checketype args (kt, expr)) >>= (\expr_type ->
--        fieldLookup kt expr_type name) -- T-Field

--checketype args (kt, e@(Arg name)) = case (M.lookup name args) of
--        Just kName      -> Right kName
--        Nothing         -> Left ("There is no `"++name++"` variable.")

--checkmethod :: KlassTable -> KlassName -> MethodName -> Either String ()
--checkmethod kt kName name = do
--        klass <- klassLookup kt kName
--        method <- methodLookup klass name
--        let method_args = mArgs method
--        let args = M.insert "this" kName (M.fromList method_args)
--        expr_type <- checketype args (kt, mExpr method)
--        mtypeRes <- mtype kt (kParent klass) name
--        C.void $ case mtypeRes of
--                Nothing                 -> isSubtypeOf kt expr_type (mRetType method)
--                Just(mArgs', mRetType') -> do
--                        subtypeCond <- isSubtypeOf kt expr_type (mRetType method)
--                        rettypeCond <- return$ mRetType' == (mRetType method)
--                        equalCond <- return$ equal_types mArgs' (map snd $ mArgs method)
--                        return$ subtypeCond && rettypeCond && equalCond

--checkclass :: KlassTable -> KlassName -> Either String ()
--checkclass kt name = do
--        klass <- klassLookup kt name
--        mapM_ (checkmethod kt name) (M.keys $ kMethods klass)

--typechecker :: Program -> IO Bool
--typechecker prog@(kt, expr) = do
--        expr_ok <- case (checketype M.empty example1) of
--                Left err        -> putStrLn ("Error in expression: "++err) >> return False
--                Right etype     -> putStrLn ("Expression type: "++etype++" - expression OK.") >> return True
--        class_ok <- C.foldM (\ok name -> case (checkclass kt name) of
--                        Left e  -> putStrLn ("Error in class `"++name++"`: "++e) >> return False
--                        Right _ -> putStrLn ("Class `"++name++"` OK.") >> return (True && ok)
--                ) True (M.keys kt)
--        return$ expr_ok && class_ok

---- Evaluator -- assumes, that typechecker was run before
---- trzeba tu poprawic rzeczy, ktore nie robi typechecker
---- zmienic na curried wersje
--evaluate :: M.Map ObjectName Object -> Program -> Object
--evaluate args prog@(kt, NewObject kName es) = Object kName os
--        where   fs = getRight $ fieldNames kt kName
--                os = map run_arg $ zip fs es
--                run_arg (name, expr) = (name, evaluate args (kt, expr))

--evaluate args prog@(kt, TypeCast expr name) = Object name os
--        where   Object _ os = evaluate args (kt, expr)

--evaluate args prog@(kt, FieldCall expr name) = fromJust $ lookup name os
--        where   Object _ os = evaluate args (kt, expr)

--evaluate args prog@(kt, MethodCall expr name es) = evaluate newArgs' (kt, mExpr)
--        where   o@(Object kName os) = evaluate args (kt, expr)
--                est = map (\e -> evaluate args (kt, e)) es
--                Method _ mArgs mExpr mRetType = fromJust $ getRight $ methodLookup' kt kName name
--                newArgs = foldl (\acc (name, val) -> M.insert name val acc) args (zip (map fst mArgs) est)
--                newArgs' = M.insert "this" o newArgs

--evaluate args prog@(kt, Arg name) = case (M.lookup name args) of
--        Just obj        -> obj
--        Nothing         -> error ("There is no `"++name++"` variable.")



---- Type erasure

---- Examples
--example_kt_pair = M.fromList [("A", Klass "A" "Object" [] (M.fromList [])),                        ("B", Klass "B" "Object" [] (M.fromList [])),                        ("Pair", Klass "Pair" "Object" [("fst", "Object"), ("snd", "Object")] (M.fromList [("setfst", Method "setfst" [("newfst", "Object")] (NewObject "Pair" [Arg "newfst", FieldCall (Arg "this") "snd"]) "Pair")]))                ]
--example1 = (example_kt_pair, expr)
--        where   expr = MethodCall (NewObject "Pair" [NewObject "A" [], NewObject "B" []]) "setfst" [NewObject "B" []]

---- Main
--main = do
--        res <- typechecker example1
--        if res
--        then putStrLn $ show $ evaluate M.empty example1
--        else return$ ()