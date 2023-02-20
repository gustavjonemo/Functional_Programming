module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
data Statement =
    Assignment String Expr.T
    | Skip
    | Begin [Statement] 
    | If Expr.T Statement Statement
    | While Expr.T Statement
    | Read String
    | Write Expr.T
    | Comment
    deriving Show


assignment, skip, begin, ifStatement, while, readStatement, write, comment :: Parser Statement

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

skip = accept "skip" #- require ";" >-> buildSkip
buildSkip _ = Skip 

begin = accept "begin" -# iter parse #- require "end" >-> Begin

ifStatement = accept "if" -# Expr.parse #- 
              require "then" # parse #- 
              require "else" # parse >-> buildIf
buildIf ((e, s1), s2) = If e s1 s2

while = accept "while" -# Expr.parse #- 
        require "do" # parse >-> buildWhile
buildWhile (e, s) = While e s

readStatement = accept "read" -# word #- require ";" >-> Read

write = accept "write" -# Expr.parse #- require ";" >-> Write

comment = accept "--" #- iter (char ? (/='\n')) #- require "\n" >-> buildComment
buildComment _ = Comment


exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec [] _ _ = []
--Assignment
exec (Assignment str expr : stmts) dict input = exec stmts (Dictionary.insert (str, Expr.value expr dict) dict) input
--Skip
exec (Skip : stmts) dict input = exec stmts dict input
--Begin
exec (Begin beginStmts : stmts) dict input = exec (beginStmts ++ stmts) dict input
--If
exec (If cond thenStmts elseStmts: stmts) dict input = 
    if (Expr.value cond dict)>0 
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input
-- While
exec (While cond stmt : stmts) dict input = 
    if Expr.value cond dict > 0
    then exec (stmt : While cond stmt : stmts) dict input
    else exec stmts dict input
--Read
exec (Read str : stmts) dict input = exec stmts (Dictionary.insert (str, head input) dict) $tail input
--Write
exec (Write expr : stmts) dict input = Expr.value expr dict : exec stmts dict input
--Comment
exec (Comment : stmts) dict input = exec stmts dict input


statement :: Parser Statement
statement = assignment ! skip ! begin ! ifStatement ! while ! readStatement ! write ! comment


indent n = if n > 0 then "    " ++ indent (n-1) else ""

shw :: Int -> Statement -> String
shw n Skip                        = indent n ++ "skip;\n"
shw n Comment                     = indent n ++ "--a comment;\n"
shw n (Read var)                  = indent n ++ "read " ++ var ++ ";\n"
shw n (Write expr)                = indent n ++ "write " ++ Expr.toString expr ++ ";\n"
shw n (Assignment var expr)       = indent n ++ var ++ " := " ++ Expr.toString expr ++ ";\n"
shw n (If cond thenStmt elseStmt) = indent n ++ "if " ++ Expr.toString cond ++ " then\n" ++
                                                        shw (n+1) thenStmt ++ indent n ++ "else\n" ++ shw (n+1) elseStmt
shw n (While cond stmt)           = indent n ++ "while " ++ Expr.toString cond ++ " do\n" ++ shw (n+1) stmt
shw n (Begin stmts)               = indent n ++ "begin\n" ++ concatMap (shw (n+1)) stmts ++ indent n ++ "end\n"

instance Parse Statement where
  parse = statement
  toString = shw 0