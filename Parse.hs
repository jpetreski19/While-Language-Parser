{-
  Parser for a simple while language

  parseExp parses a string as expression of type Exp (defined in While.hs).
  parseCom parses a string as command of type Com (defined in While.hs).

  Currently, when the parser fails, it does not produce
  an error message.
  To augment it to print the cause of the error, parser of type

    String -> Either String a

   can be used.
-}

module Parse (parseExp,parseCom) where
import While

import PComb
import Control.Applicative( (<|>) )

{-
  Expression parser

  Makes use of helper parsers: ctrl, ident and number from PComb.hs:
    1. ctrl c - ignores whitespaces and tries to parse character c,
    2. ident - ignores whitespaces and tries to parse a random string,
    3. number - ignores whitespaces and tries to parse an integer.

  The helper parsers are orderd by their precedence from first to last:
    the operations with hgher precedence appear above the ones with lower.

  Whenever an operation of type t is being parsed, it is considered as
  a binary operation of results of operations of higher precedence.
  Uminus is an exception here, where it can have either a variable,
  constant of a term.


  To maintain left associativity when there are no brackets, the operations
  with same precedence are stored in a list of pairs where
    1st element is the operation type,
    2nd is the result of performing the next operation recursively.

  The logic behind the parsing is the following:
    Try parsing first operand, then try all operations of same precedence
    (including the operands in between).
    This will either succeed or evaluate to either parsing a variable or constant.
    Therefore, the recursion will always terminate.

  Then, the leftAssoc function is used when doing fold left, which gives
  the desired format (property of foldl).


  If a parser of an operation fails, then it tries to parse it with a parser
  for operation with higher precedence, up until parsing an atom. If an atom can
  be parsed as variable or constant, then it stops there. Otherwise, it recursively
  tries to parse a term.

  The parsing of each operation is denoted as parsei where i is the precedence.
    parse1 - times and divide
    parse2 - plus and minus
    parse3 - less, less than or equal, equal
    parse4 - and
    parseTerm - or (An exception in naming here).
-}
parseExp :: String -> Maybe Exp
parseExp s = parseAll myExpP s
myExpP = parseTerm

parseName = do
  name<-ident;
  return (Var name)

parseConst = do
  val<-number;
  return (Const val)

parseAtom = (do ctrl "("; r<-parseTerm; ctrl ")"; return r)
    <|> parseName 
    <|> parseConst

-- *~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
-- Uminus
parseUminus = (do ctrl "-"; r<-parseName; return (Uminus r))
    <|> (do ctrl "-"; ctrl "("; r<-parseConst; ctrl ")"; return (Uminus r))
    <|> (do ctrl "-"; ctrl "("; r<-parseTerm; ctrl ")"; return (Uminus r))
    <|> parseAtom

-- *~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
-- Times and Div
parse1 = do
  a<-parseUminus
  bs<-many op
  return (foldl leftAssoc a bs) where
    op = do ctrl "*"; r<-parseUminus; return (Times, r)
      <|> do ctrl "/"; r<-parseUminus; return (Div, r)

-- *~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
-- Plus and Minus
parse2 = do
  a<-parse1
  bs<-many op
  return (foldl leftAssoc a bs) where
    op = do ctrl "+"; r<-parse1; return (Plus, r)
      <|> do ctrl "-"; r<-parse1; return (Minus, r)

-- *~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
-- Less, LessEq and Equal
parse3 = do
  a<-parse2
  bs<-many op
  return (foldl leftAssoc a bs) where
    op = do ctrl "<"; r<-parse2; return (Less, r)
      <|> do ctrl "<="; r<-parse2; return (LessEq, r)
      <|> do ctrl "=="; r<-parse2; return (Equal, r)

-- *~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
-- And
parse4 = do
  a<-parse3
  bs<-many op
  return (foldl leftAssoc a bs) where
    op = do ctrl "&&"; r<-parse3; return (And, r)

-- *~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
-- Or
parseTerm = do
  a<-parse4
  bs<-many op
  return (foldl leftAssoc a bs) where
    op = do ctrl "||"; r<-parse4; return (Or, r)

{-
  Helper function

  Maintains left associativity when there are
  no brackets. Used as function in foldl.
-}
leftAssoc a (op, b) = Binop op a b





{-
  Command parser

  Helper parsers: ctrl, sepBy, ident and keyword
  from PComb.hs are used:

    ctrl c - ignores whitespaces and looks for character c

    sepBy p c - parse strings using parser p, but before that,
                separate the original string into list of strings
                such that string ends if character c is parsed.

    ident - ignores whitespaces and parses a random string.

    keyword k - ignores whitespaces and tries to match string
                k with a prefix of the original string.
-}
parseCom :: String -> Maybe Com
parseCom s = parseAll pCom s
{-
  Try any of the parsers one by one. If one matches, return the result.
  Otherwise, return Nothing (failure).
-}
pCom = parseAssign <|> parseIfElse <|> parseWhile <|> parseSeq

{-
  Assign command

  Format:
    random string = expression

    e.g.
      "a=5"
      "b=(a+b)/c&&d"

  Logic:
    1. Try to parse ident (a string),
    2. Try to parse "=",
    3. Try to parse expression,
    4. Return command data type.

    (If one of the helper parsers fails,
    the whole parser fails).
-}
parseAssign = do
  name<-ident;
  ctrl "=";
  exp<-myExpP;
  return (Assign name exp)

{-
  If Else command

  Format:
    "if cond then com else com" where
    cond is expression and com is command.

  Logic:
    1. Try to parse keyword "if",
    2. Try to parse the condition (an expression),
    3. Recusrively try to parse the command in the if block,
    4. Try to parse keyword "else",
    5. Recursively parse the command in the else block.
-}
parseIfElse = do
  keyword "if";
  cond<-myExpP;
  keyword "then";
  a<-pCom;
  keyword "else";
  b<-pCom;
  return (If cond a b)

{-
  While command

  Format:
    "while cond do com" where cond is expression and com is 
    command (body of the while loop).

  Logic:
    1. Try to parse keyword "while",
    2. Try to parse the condition (an expression),
    3. Try to parse keyword "do",
    4. Recursively try to parse the command in the body.
-}
parseWhile = do
  keyword "while";
  cond<-myExpP;
  keyword "do";
  body<-pCom;
  return (While cond body)

{-
  Seq [Com] command

  Format:
    "{ com_1; com_2; ... com_n }" where com_i (1 <= i <= n) is a command.

  Logic:
    1. Try to parse character "{",
    2. Make use of the sepBy helper parser in PComb.hs
       to get a list of commands separated by ";",
    3. Recursively try to parse the command(s) in the block,
    4. Try to parse character "}".
-}
parseSeq = do
  ctrl "{";
  coms<-sepBy pCom (ctrl ";");
  ctrl "}";
  return (Seq coms)


