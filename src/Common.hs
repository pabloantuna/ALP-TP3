module Common where

  -- Comandos interactivos o de archivos
  data Stmt i = Def String i           --  Declarar un nuevo identificador x, let x = t
              | Eval i                 --  Evaluar el término
    deriving (Show)
  
  instance Functor Stmt where
    fmap f (Def s i) = Def s (f i)
    fmap f (Eval i)  = Eval (f i)

  -- Tipos de los nombres
  data Name
     =  Global  String
    deriving (Show, Eq)

  -- Entornos
  type NameEnv v t = [(Name, (v, t))]

  -- Tipo de los tipos
  data Type = EmptyT
            | FunT Type Type
            | NatT 
            | ListNat
            deriving (Show, Eq)
  
  -- Términos con nombres
  data LamTerm  =  LVar String
                |  LAbs String Type LamTerm
                |  LApp LamTerm LamTerm
                |  LLet String LamTerm LamTerm
                |  LZero
                |  LSuc LamTerm
                |  LR LamTerm LamTerm LamTerm
                |  LNil
                |  LCons LamTerm LamTerm
                |  LRL LamTerm LamTerm LamTerm
       deriving (Show, Eq)


  -- Términos localmente sin nombres
  data Term  = Bound Int
             | Free Name 
             | Term :@: Term
             | Lam Type Term
             | Let Term Term
             | Zero
             | Suc Term
             | R Term Term Term
             | Nil
             | Cons Term Term
             | RL Term Term Term
       deriving (Show, Eq)

  -- Valores
  data Value = VLam Type Term 
             | VNat NV
             | VList LV
       deriving (Show, Eq)

  data NV = NVZero | NVSucc NV
       deriving (Show, Eq)

  data LV = LVNil | LVCons Value LV
       deriving (Show, Eq)
  -- Contextos del tipado
  type Context = [Type]
