{
module Parse where
import Common
import Data.Maybe
import Data.Char

}

%monad { P } { thenP } { returnP }
%name parseStmt Def
%name parseStmts Defs
%name term Exp

%tokentype { Token }
%lexer {lexer} {TEOF}

%token
    '='            { TEquals }
    ':'            { TColon }
    '\\'           { TAbs }
    '.'            { TDot }
    '('            { TOpen }
    ')'            { TClose }
    '->'           { TArrow }
    VAR            { TVar $$ }
    TYPEE          { TTypeE }
    DEF            { TDef }
    LET            { TLet }
    IN             { TIn }
    TYPENAT        { TTypeNat}
    ZERO           { TZero }
    SUC            { TSuc }
    R              { TR }
    NUM            { TNum $$ }
    TYPELISTNAT    { TTypeListNat }
    NIL            { TNil }
    CONS           { TCons }
    RL             { TRL }
    '['            { TOpenBracket }
    ']'            { TCloseBracket }
    ','            { TComma }
    

%right VAR
%left '=' 
%right '->'
%right '\\' '.' LET IN
%right R
%right RL
%right CONS
%right SUC

%%

Def     :  Defexp                      { $1 }
        |  Exp	                       { Eval $1 }
Defexp  : DEF VAR '=' Exp              { Def $2 $4 } 

Exp     :: { LamTerm }
        : '\\' VAR ':' Type '.' Exp    { LAbs $2 $4 $6 }
        | LET VAR '=' Exp IN Exp       { LLet $2 $4 $6 }
        | SUC Exp                      { LSuc $2 }
        | R Atom Atom Atom             { LR $2 $3 $4 }
        | CONS Atom Atom               { LCons $2 $3 }
        | RL Atom Atom Atom            { LRL $2 $3 $4 }
        | NAbs                         { $1 }

NAbs    :: { LamTerm }
        : NAbs Atom                    { LApp $1 $2 }
        | Atom                         { $1 }

Atom    :: { LamTerm }
        : VAR                          { LVar $1 }
        | NUM                          { fromNat $1 }
        | '(' Exp ')'                  { $2 }
        | '[' Arr ']'                  { $2 }
        | ZERO                         { LZero }
        | NIL                          { LNil }

Arr     :: { LamTerm }
        :                              { LNil }
        | Exp                          { LCons $1 LNil }
        | Exp ',' Arr                  { LCons $1 $3 }

Type    : TYPEE                        { EmptyT }
        | TYPENAT                      { NatT }
        | TYPELISTNAT                  { ListNat }
        | Type '->' Type               { FunT $1 $3 }
        | '(' Type ')'                 { $2 }

Defs    : Defexp Defs                  { $1 : $2 }
        |                              { [] }
     
{

data ParseResult a = Ok a | Failed String
                     deriving Show                     
type LineNumber = Int
type P a = String -> LineNumber -> ParseResult a

getLineNo :: P LineNumber
getLineNo = \s l -> Ok l

thenP :: P a -> (a -> P b) -> P b
m `thenP` k = \s l-> case m s l of
                         Ok a     -> k a s l
                         Failed e -> Failed e
                         
returnP :: a -> P a
returnP a = \s l-> Ok a

failP :: String -> P a
failP err = \s l -> Failed err

catchP :: P a -> (String -> P a) -> P a
catchP m k = \s l -> case m s l of
                        Ok a     -> Ok a
                        Failed e -> k e s l

happyError :: P a
happyError = \ s i -> Failed $ "Línea "++(show (i::LineNumber))++": Error de parseo\n"++(s)

fromNat :: Int -> LamTerm
fromNat 0 = LZero
fromNat x = LSuc $ fromNat (x-1)

data Token = TVar String
               | TNum Int
               | TTypeE
               | TDef
               | TAbs
               | TDot
               | TOpen
               | TClose 
               | TColon
               | TArrow
               | TEquals
               | TLet
               | TIn
               | TTypeNat
               | TZero
               | TSuc
               | TR
               | TTypeListNat
               | TNil
               | TCons
               | TRL
               | TOpenBracket
               | TCloseBracket
               | TComma
               | TEOF
               deriving Show

----------------------------------
lexer cont s = case s of
                    [] -> cont TEOF []
                    ('\n':s)  ->  \line -> lexer cont s (line + 1)
                    (c:cs)
                          | isSpace c  -> lexer cont cs
                          | isAlpha c  -> lexVar (c:cs)
                          | isNumber c -> lexNat (c:cs)
                    ('0':cs) -> cont TZero cs
                    ('-':('-':cs)) -> lexer cont $ dropWhile ((/=) '\n') cs
                    ('{':('-':cs)) -> consumirBK 0 0 cont cs	
                    ('-':('}':cs)) -> \ line -> Failed $ "Línea "++(show line)++": Comentario no abierto"
                    ('-':('>':cs)) -> cont TArrow cs
                    ('\\':cs)-> cont TAbs cs
                    ('.':cs) -> cont TDot cs
                    ('(':cs) -> cont TOpen cs
                    ('-':('>':cs)) -> cont TArrow cs
                    (')':cs) -> cont TClose cs
                    (':':cs) -> cont TColon cs
                    ('=':cs) -> cont TEquals cs
                    ('[':cs) -> cont TOpenBracket cs
                    (']':cs) -> cont TCloseBracket cs
                    (',':cs) -> cont TComma cs
                    unknown -> \line -> Failed $ 
                     "Línea "++(show line)++": No se puede reconocer "++(show $ take 10 unknown)++ "..."
                    where lexVar cs = case span isAlpha cs of
                              ("E",rest)    -> cont TTypeE rest
                              ("def",rest)  -> cont TDef rest
                              ("let",rest)  -> cont TLet rest
                              ("in",rest)  -> cont TIn rest
                              ("Nat",rest) -> cont TTypeNat rest
                              ("R", rest) -> cont TR rest
                              ("Suc", rest) -> cont TSuc rest
                              ("ListNat", rest) -> cont TTypeListNat rest
                              ("Nil", rest) -> cont TNil rest
                              ("Cons", rest) -> cont TCons rest
                              ("RL", rest) -> cont TRL rest
                              (var,rest)    -> cont (TVar var) rest
                          consumirBK anidado cl cont s = case s of
                              ('-':('-':cs)) -> consumirBK anidado cl cont $ dropWhile ((/=) '\n') cs
                              ('{':('-':cs)) -> consumirBK (anidado+1) cl cont cs	
                              ('-':('}':cs)) -> case anidado of
                                                  0 -> \line -> lexer cont cs (line+cl)
                                                  _ -> consumirBK (anidado-1) cl cont cs
                              ('\n':cs) -> consumirBK anidado (cl+1) cont cs
                              (_:cs) -> consumirBK anidado cl cont cs
                          lexNat cs = case span isNumber cs of
                              (num, rest) -> cont (TNum $ read num) rest
                                           
stmts_parse s = parseStmts s 1
stmt_parse s = parseStmt s 1
term_parse s = term s 1
}
