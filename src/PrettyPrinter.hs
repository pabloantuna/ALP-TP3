module PrettyPrinter
  ( printTerm
  ,     -- pretty printer para terminos
    printType     -- pretty printer para tipos
  )
where

import           Common
import           Text.PrettyPrint.HughesPJ
import           Prelude                 hiding ( (<>) )
-- lista de posibles nombres para variables
vars :: [String]
vars =
  [ c : n
  | n <- "" : map show [(1 :: Integer) ..]
  , c <- ['x', 'y', 'z'] ++ ['a' .. 'w']
  ]

parensIf :: Bool -> Doc -> Doc
parensIf True  = parens
parensIf False = id

-- pretty-printer de términos

pp :: Int -> [String] -> Term -> Doc
pp ii vs (Bound k         ) = text (vs !! (ii - k - 1))
pp _  _  (Free  (Global s)) = text s

pp ii vs (i :@: c         ) = sep
  [ parensIf (isLam i || isLet i || isR i) (pp ii vs i)
  , nest 1 (parensIf (isNotVal c) (pp ii vs c))
  ]
pp ii vs (Lam t c) =
  text "\\"
    <> text (vs !! ii)
    <> text ": "
    <> printType t
    <> text ". "
    <> pp (ii + 1) vs c
pp ii vs (Let t1 t2) =
  sep [text "let"
    , text (vs !! ii)
    , text "="
    , parensIf (isNotVal t1) (pp ii vs t1)
    , text "in"
    , parensIf (isNotVal t1) (pp (ii + 1) vs t2)]
pp ii vs Zero = text "0"
pp ii vs s@(Suc n) = if isNat n then int $ nat2Int s else text "Suc " <> parensIf (isNotVal n) (pp ii vs n)
pp ii vs (R t1 t2 t3) = sep [
  text "R"
    , parensIf (isNotVal t1) (pp ii vs t1)
    , parensIf (isNotVal t2) (pp ii vs t2)
    , parensIf (isNotVal t3) (pp ii vs t3)]
pp ii vs Nil = text "[]"
pp ii vs c@(Cons x xs) = if isList c
  then (brackets . hsep . punctuate comma . map int . natList2intList) c
  else sep [text "Cons", parensIf (isNotVal x) $ pp ii vs x, parensIf (isNotVal x) $ pp ii vs xs]
pp ii vs (RL t1 t2 t3) = sep [
  text "RL"
    , parensIf (isNotVal t1) (pp ii vs t1)
    , parensIf (isNotVal t2) (pp ii vs t2)
    , parensIf (isNotVal t3) (pp ii vs t3)]

nat2Int :: Term -> Int
nat2Int Zero = 0
nat2Int (Suc n) = 1 + nat2Int n
nat2Int _ = error "El valor no es un natural"

natList2intList :: Term -> [Int]
natList2intList Nil = []
natList2intList (Cons x xs) = nat2Int x : natList2intList xs
natList2intList _ = error "El valor no es una lista"

isLam :: Term -> Bool
isLam (Lam _ _) = True
isLam _         = False

isApp :: Term -> Bool
isApp (_ :@: _) = True
isApp _         = False

isLet :: Term -> Bool
isLet (Let _ _) = True
isLet _ = False

isR :: Term -> Bool
isR R {} = True
isR _    = False

isNat :: Term -> Bool 
isNat Zero = True 
isNat (Suc n) = isNat n
isNat _ = False

isList :: Term -> Bool
isList Nil = True
isList (Cons x xs) = isNat x && isList xs
isList _ = False

isNotVal :: Term -> Bool
isNotVal t = isLam t || isApp t || isR t || isLet t


-- pretty-printer de tipos
printType :: Type -> Doc
printType EmptyT = text "E"
printType (FunT t1 t2) =
  sep [parensIf (isFun t1) (printType t1), text "->", printType t2]
printType NatT = text "Nat"
printType ListNat = text "[Nat]"


isFun :: Type -> Bool
isFun (FunT _ _) = True
isFun _          = False

fv :: Term -> [String]
fv (Bound _         ) = []
fv (Free  (Global n)) = [n]
fv (t   :@: u       ) = fv t ++ fv u
fv (Lam _   u       ) = fv u
fv (Let t1 t2)        = fv t1 ++ fv t2
fv Zero               = []
fv (Suc te)           = fv te
fv (R te te' te2)     = fv te ++ fv te' ++ fv te2
fv Nil                = []
fv (Cons te te')      = fv te ++ fv te'
fv (RL te te' te2)    = fv te ++ fv te' ++ fv te2

---
printTerm :: Term -> Doc
printTerm t = pp 0 (filter (\v -> v `notElem` (fv t)) vars) t

