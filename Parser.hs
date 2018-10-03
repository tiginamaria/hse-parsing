module Parser where

import Tokenizer
import Prelude hiding (lookup)

data AST = APower Operator AST AST
         | AUminus AST
         | ASum Operator AST AST
         | AProd Operator AST AST
         | AAssign String AST
         | ANum Integer
         | AIdent String

parse :: String -> Maybe AST
parse input =
  let ts = tokenize input in
  case ts of
    [TEof] -> Nothing
    _ -> let (tree, ts') = expr ts in
         if ts' == [TEof]
         then Just tree
         else error ("Parsing error on: " ++ show ts')

{-<expr> -> <term> <term_tail>-}         
expr :: [Token] -> (AST, [Token])
expr ts =
  let (termNode, ts') = term ts in
  case lookup ts' of
    TOp op | op == Plus || op == Minus ->
      let (exprNode', ts'') = term_tail termNode ts'
      in (exprNode', ts'')
    TAssign ->
      case termNode of
        AIdent v -> let (exprNode, ts'') = expr (accept ts') 
                    in (AAssign v exprNode, ts'')
        _ -> error "Syntax error: assignment is only possible to identifiers"
    _ -> (termNode, ts')
    
{-<term_tail1> -> '+|-' <term> <term_tail2> | empty-} 
term_tail :: AST -> [Token] -> (AST, [Token])
term_tail termNode ts =
  case lookup ts of
    TOp op | elem op [Plus, Minus] ->
      let (termNode', ts') = term (accept ts) in 
      let (term_tailNode, ts'') = term_tail (ASum op termNode termNode') ts' 
      in (term_tailNode, ts'')
    _ -> (termNode, ts)

{-<term> -> <pow> <pow_tail> | empty-}     
term :: [Token] -> (AST, [Token])
term ts =
  let (powNode, ts') = pow ts in
  case lookup ts' of
    TOp op | elem op [Mult, Div] ->
      let (pow_tailNode, ts'') = pow_tail powNode ts'
      in (pow_tailNode, ts'')
    _ -> (powNode, ts')

{-<pow> -> <factor> '^' <pow> | empty-} 
pow :: [Token] -> (AST, [Token])
pow ts =
  let (factNode, ts') = factor ts in
  case lookup ts' of
    TOp op | op == Power ->
      let (powNode, ts'') = pow (accept ts') 
      in (APower op factNode powNode, ts'')
    _ -> (factNode, ts')

{-<pow_tail1> -> '*|/' <pow> <pow_tail2> | empty-} 
pow_tail :: AST -> [Token] -> (AST, [Token])
pow_tail powNode ts =
  case lookup ts of
    TOp op | elem op [Mult, Div] ->
      let (powNode', ts') = term (accept ts) in 
      let (pow_tailNode, ts'') = pow_tail (AProd op powNode powNode') ts' 
      in (pow_tailNode, ts'')
    _ -> (powNode, ts)

{-<factor> -> '(' <expr> ')' |'-' <pow> | digit | ident-}   
factor :: [Token] -> (AST, [Token])
factor ts =
  case lookup ts of
    TLParen ->
      let (exprNode, ts') = expr (accept ts) in
      case lookup ts' of
        TRParen -> (exprNode, accept ts')
        _ -> error "Syntax error: mismatched parentheses"
    TIdent v -> (AIdent v, accept ts)
    TDigit d -> (ANum d, accept ts)
    TOp op | op == Minus ->
      let (powNode, ts') = pow (accept ts) 
      in (AUminus powNode, ts')
    _ -> error "Syntax error: factor can only be a digit, an identifier or a parenthesised expr"

lookup :: [Token] -> Token
lookup = head

accept :: [Token] -> [Token]
accept = tail

instance Show AST where
  show tree = "\n" ++ show' 0 tree
    where
      show' n t =
        (if n > 0 then \s -> concat (replicate (n - 1) "| ") ++ "|_" ++ s else id)
        (case t of
                  ASum  op l r -> showOp op : "\n" ++ show' (ident n) l ++ "\n" ++ show' (ident n) r
                  AProd op l r -> showOp op : "\n" ++ show' (ident n) l ++ "\n" ++ show' (ident n) r
                  AAssign  v e -> v ++ " =\n" ++ show' (ident n) e
                  ANum   i     -> show i
                  AIdent i     -> i
                  AUminus r    -> showOp Minus : "\n" ++ show' (ident n) r
                  APower op l r -> showOp op : "\n" ++ show' (ident n) l ++ "\n" ++ show' (ident n) r)
               
      ident = (+1)
      showOp Plus  = '+'
      showOp Minus = '-'
      showOp Mult  = '*'
      showOp Div   = '/'
      showOp Power = '^'  
