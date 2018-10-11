module Parser (parse) where -- only expose the top-level parsing function

import Combinators
import qualified Tokenizer as T
import Prelude hiding (lookup, (>>=), map, pred, return, elem, spaces)

data AST = ASum T.Operator AST AST
         | AProd T.Operator AST AST
         | APow T.Operator AST AST
         | AAssign String AST
         | ANum Integer
         | AUminus AST
         | AIdent String
         | AList [AST]
         | ALConcat String AST AST
                
parse :: Parser [AST]
parse =
    ( eoinput |> return ([]))
    <|> expression >>= \e ->
          ( eoinput |> return([e]))
      <|> ( eoexpr |>
            parse >>= \tree -> return([e] ++ tree)
          )

expression :: Parser AST
expression = mathexpr
         <|> listexpr
  
listexpr :: Parser AST
listexpr =
  (     identifier >>= \(AIdent i) -> (
        ( assignment |>
          listexpr >>= \le -> return (AAssign i le)
        )
    <|> ( concatL  >>= \op -> 
          listexpr >>= \le -> return (ALConcat "++" (AIdent i) le)
        )
    <|> return (AIdent i) )
  )
  <|> ( lsqparen |>
        list     >>= \l ->
        ( concatL  >>= \op -> 
          listexpr >>= \le -> return (ALConcat "++" (AList l) le)
        )
        <|> return (AList l)
      )  
      
list :: Parser [AST]
list = 
        expression >>= \e ->
        ( (comma |> 
        list >>= \l -> return ([e] ++ l))
    <|> (rsqparen |> return([e])) )

mathexpr :: Parser AST
mathexpr =
  ( identifier >>= \(AIdent i) ->
    assignment |>
    expression >>= \e -> return (AAssign i e)
  )
  <|> ( term       >>= \l  -> -- Here the identifier is parsed twice :(
        plusMinus  >>= \op ->
        expression >>= \r  -> return (ASum op l r)
      )
  <|> term  

term :: Parser AST
term =
  power >>= \l ->
  ( ( divMult >>= \op ->
      term    >>= \r  -> return (AProd op l r)
    )
    <|> return l
  )

power :: Parser AST
power =
  factor >>= \l ->
  ( ( pow   >>= \op ->
      power >>= \r  -> return (APow op l r)
    )
    <|> return l
  )

factor :: Parser AST
factor =
  ( lparen |>
    expression >>= \e ->
    rparen |> return e -- No need to keep the parentheses
  )
  <|> identifier
  <|> number
  <|> ( uMinus >>= \op ->
          power  >>= \r  -> return (AUminus r)
      )

number :: Parser AST
number     = map (ANum   . T.num) (get T.isDigit)

identifier :: Parser AST
identifier = map (AIdent . T.ident) (get T.isAlpha)

lparen :: Parser Char
lparen = char '('

rparen :: Parser Char
rparen = char ')'

lsqparen :: Parser Char
lsqparen = char '['

rsqparen :: Parser Char
rsqparen = char ']'

assignment :: Parser Char
assignment = char '='

eoinput :: Parser [AST]
eoinput = eof
  
eoexpr:: Parser Char
eoexpr = char ';'

comma :: Parser Char
comma = char ','

concatL :: Parser Char
concatL = (char '+' |> char '+')

plusMinus :: Parser T.Operator
plusMinus = map T.operator (char '+' <|> char '-')

uMinus :: Parser T.Operator
uMinus = map T.operator (char '-')

divMult :: Parser T.Operator
divMult   = map T.operator (char '/' <|> char '*')

pow :: Parser T.Operator
pow  = map T.operator (char '^')

instance Show AST where
  show tree = "\n" ++ show' 0 tree
    where
      show' n t =
        (if n > 0 then \s -> concat (replicate (n - 1) "| ") ++ "|_" ++ s else id)
        (case t of
                  ASum  op l r -> showOp op : "\n" ++ show' (ident n) l ++ "\n" ++ show' (ident n) r
                  AProd op l r -> showOp op : "\n" ++ show' (ident n) l ++ "\n" ++ show' (ident n) r
                  AAssign v e -> v ++ " =\n" ++ show' (ident n) e
                  ANum   i     -> show i
                  AUminus r    -> showOp T.Minus : "\n" ++ show' (ident n) r
                  APow op l r -> showOp op : "\n" ++ show' (ident n) l ++ "\n" ++ show' (ident n) r
                  AIdent i     -> show i
                  AList l -> show l
                  ALConcat op l r  -> op ++ "\n" ++ show' (ident n) l ++ "\n" ++ show' (ident n) r)
      ident = (+1)
      showOp T.Plus  = '+'
      showOp T.Minus = '-'
      showOp T.Mult  = '*'
      showOp T.Div   = '/'
      showOp T.Pow   = '^'
