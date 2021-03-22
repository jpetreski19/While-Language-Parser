{-
  Expressions:
  * No parenthesis if given by operator priority, e.g.
    a*b+c instead of (a*b)+c

  * All operators associate to the left!
    E.g., print:
      a-b-c instead of (a-b)-c
      a+b+c instead of (a+b)+c


  Commands:
  * Separate commands in sequential composition by ;, use { and } as delimiters

    * insert line breaks after: do, then, else, command in sequential composition
      * Exception: do not insert line-breaks between {,} and then,else,do
      * Exception: do not insert line break between else and if

    * 2 spaces per level of indentation.

    * Delimit all Seq-blocks with {}, even if nested

-}
module Pretty (prettyExp, prettyCom) where
import While

{-
  Expression pretty printer

  Additional argument is used to keep track of the priority of the operations.
  Each operation has a priority value assigned to it:
    1: ||
    2: &&
    3: <, <=, ==
    4: +, -
    5: *, /

  If an operation of lower precedence is part of operation with higher precedence,
  put parentheses.

  If an operation is part of an operation with the same precedence, and it is the
  second operand, then put parentheses, in order to capture cases such as:
      3 + (4 - 5)   or    a <= (b == c)
-}
prettyExp :: Exp -> String
prettyExp e = pExp 0 e where
  pExp _ (Var name) = name
  pExp _ (Const val) = show val
  pExp _ (Uminus (Var x)) = "-" ++ x
  pExp _ (Uminus exp) = "-(" ++ pExp 0 exp ++ ")"

  -- Or
  pExp priority (Binop Or a b)  | (priority < 1)  = pExp 0 a ++ "||" ++ pExp 1 b
                                | otherwise       = "(" ++ pExp 0 a ++ "||" ++ pExp 1 b ++ ")"
  -- And
  pExp priority (Binop And a b) | (priority < 2)  = pExp 1 a ++ "&&" ++ pExp 2 b
                                | otherwise       = "(" ++ pExp 1 a ++ "&&" ++ pExp 2 b ++ ")"

  -- Equal
  pExp priority (Binop Equal a b) | (priority < 3)  = pExp 2 a ++ "==" ++ pExp 3 b
                                  | otherwise       = "(" ++ pExp 2 a ++ "==" ++ pExp 3 b ++ ")"
  -- Less than or equal
  pExp priority (Binop LessEq a b)  | (priority < 3)  = pExp 2 a ++ "<=" ++ pExp 3 b
                                    | otherwise       = "(" ++ pExp 2 a ++ "<=" ++ pExp 3 b ++ ")"
  -- Less
  pExp priority (Binop Less a b)  | (priority < 3)  = pExp 2 a ++ "<" ++ pExp 3 b
                                  | otherwise       = "(" ++ pExp 2 a ++ "<" ++ pExp 3 b ++ ")"

  -- Plus
  pExp priority (Binop Plus a b)  | (priority < 4)  = pExp 3 a ++ "+" ++ pExp 4 b
                                  | otherwise       = "(" ++ pExp 3 a ++ "+" ++ pExp 4 b ++ ")"
  -- Minus
  pExp priority (Binop Minus a b) | (priority < 4)  = pExp 3 a ++ "-" ++ pExp 4 b
                                  | otherwise       = "(" ++ pExp 3 a ++ "-" ++ pExp 4 b ++ ")"

  -- Times
  pExp priority (Binop Times a b) | (priority < 5)  = pExp 4 a ++ "*" ++ pExp 5 b
                                  | otherwise       = "(" ++ pExp 4 a ++ "*" ++ pExp 5 b ++ ")"
  -- Div
  pExp priority (Binop Div a b) | (priority < 5)  = pExp 4 a ++ "/" ++ pExp 5 b
                                | otherwise       = "(" ++ pExp 4 a ++ "/" ++ pExp 5 b ++ ")"



{-
  Command pretty printer
-}
prettyCom :: Com -> String
prettyCom c = pCom "" c where
  pCom indent (Assign name exp) = indent ++ name ++ " = " ++ prettyExp exp

  {-
    Deal with If commands by splitting the command corresponding to the
    first part and the command corresponding to the second (else).

    p1 deals with a in If cond a b
      p1 Seq [Com]  -   Exception 1: no line break between then and {
      p1 com        -   Generic case

    p2 deals with b in If cond a b
      p2 Seq [Com]  -   Exception 2: no line break between else and {
      p2 If c a b   -   Exception 3: no line break between else and if
        (in case if is a command of the else block)

       *pCom2 has the same function as pCom but
        does not indent the if after else (edge case)*

      p2 com  -   Generic case
  -}
  pCom indent (If cond a b) = indent ++ "if " ++ prettyExp cond ++ " then" ++ p1 (indent ++ "  ") a ++ "else" ++ p2 (indent ++ "  ") b where
    p1 i (Seq s) = " " ++ pCom indent (Seq s) ++ " "
    p1 i com = "\n" ++ pCom i com ++ "\n" ++ indent

    p2 i (Seq s) = " " ++ pCom indent (Seq s)
    p2 i (If cond x y) = " " ++ pCom2 (If cond x y) where
      -- In this case, there should be no indentation between else and if ..., so deal with it here
      pCom2 (If cond a b) = "if " ++ prettyExp cond ++ " then" ++ p1 i a ++ "else" ++ p2 i b
    p2 i com = "\n" ++ pCom i com

  {-
    While

      p Seq [Com] - Exception 4: no line break between do and {
      p com       - Generic case
  -}
  pCom indent (While cond body) = indent ++ "while " ++ prettyExp cond ++ " do" ++ p (indent ++ "  ") body where
    p i (Seq s) = " " ++ pCom indent (Seq s)
    p i com = "\n" ++ pCom i com

  {-
    Seq [Com]
      First breackets are open, new line added and indentation adjsted,
      then the list of commands is dealt with.

        p' Seq []   - empty string
        p' Seq [c]  - Exception 5: no line break and ; after the last command
        p' Seq s    - Generic case: put ; and line break
  -}
  pCom indent (Seq []) = "{}"
  pCom indent (Seq s) = "{" ++ "\n" ++ p (indent ++ "  ") (Seq s) ++ "\n" ++ indent ++ "}" where
    p i (Seq []) = ""
    p i (Seq [c]) = pCom i c
    p i (Seq (x:xs)) = (pCom i x) ++ ";" ++ "\n" ++ (p i (Seq xs))

