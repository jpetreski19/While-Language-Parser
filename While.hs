{-
  Abstract Syntax tree defined here.

    * Binary operations: +, -, *, /, ==, <, <=, &, |
    * Expressions: value (integer), name (string), unary minus (uminus)
      and binary operation.
    * Commands: Assign, Sequence of commands, If-else, While
-}

module While where

-- Some definitions
type Name = String
type Val = Integer

-- Binary operations:
data Binop =
            Plus  | Minus | Times | Div
          | Equal | Less | LessEq
          | And | Or
  deriving (Show,Eq)

-- Exression:
data Exp =
    Var Name
  | Const Val
  | Uminus Exp
  | Binop Binop Exp Exp
  deriving (Show,Eq)


-- Commands:
data Com =
      Assign Name Exp
    | Seq [Com]
    | If Exp Com Com
    | While Exp Com
  deriving (Show,Eq)
