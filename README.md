# While-Language-Parser
Parser for a simple while language in Haskell

To run the parser, just load Parse.hs in ghci:
ghci Parse.hs

To parse an expression, call:
parseExp s

where s is the string. E.g.: "a&&-(0)--(-25)-y<-(-(8)<z)"

To parse a command, call:
parseCom s

E.g.: parseCom "if z then {\n  a = 1<z;\n  while 0 do {}\n} else\n  b = y<y"
