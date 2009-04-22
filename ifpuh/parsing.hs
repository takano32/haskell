#!/usr/bin/env hugs
import Char
-- hoge :: Num a => String -> [a]
-- hoge s = [3]
newtype Parser a = MkP (String -> [(a,String)])

apply :: Parser a -> String -> [(a,String)]
apply (MkP f) s = f s

applyParser :: Parser a -> String -> a
applyParser p = fst . head . apply p

instance Monad Parser where
    return x = MkP f where f s = [(x,s)]
    p >>= q = MkP f
        where f s = 
                  [(y,s'') | (x,s') <- apply p s, (y,s'') <- apply (q x) s']

item :: Parser Char
item = MkP f
       where f []      = []
             f (c:cs)  = [(c,cs)]

zero :: Parser a
zero  = MkP f where f s = []

sat :: (Char -> Bool) -> Parser Char
sat p = do {c <- item; if p c then return c else zero}

char :: Char -> Parser Char
char x = do {c <- sat (==x); return c }

string :: String -> Parser()
string [] = return ()
string (x:xs) = do {char x; string xs; return()}

lower :: Parser Char
lower = sat isLower

digit :: Parser Int
digit  = do {d <- sat isDigit; return(ord d - ord '0')}


plus :: Parser a -> Parser a -> Parser a
p `plus` q = MkP f where f s = apply p s ++ apply q s

plusLowers :: Parser String
plusLowers = do {c <- lower; cs <- lowers; return(c:cs)} `plus` return ""

orelseLowers :: Parser String
orelseLowers = do {c <- lower; cs <- lowers; return(c:cs)} `orelse` return ""


lowers :: Parser String
lowers  = do {c <- lower; cs <- lowers; return(c:cs)} `orelse` return ""


orelse :: Parser a -> Parser a -> Parser a
p `orelse` q = MkP f where f s = if null (apply p s) 
                                 then apply q s else apply p s
{-
  where f s = if null ps then apply q s else ps
        ps = apply p s
-}


right :: Parser Int
right = digit `plus` addition

wrong :: Parser Int
wrong = digit `orelse` addition

addition :: Parser Int
addition = do {m <- digit; char '+'; n <- digit; return (m+n) }

better :: Parser Int
better = addition `orelse` digit

best :: Parser Int
best = do {m <- digit; rest m}
rest m = do {char '+';n <- digit; return (m+n)} `orelse` return m

-- 11.3 Repetition

many :: Parser a -> Parser [a]
many p = do {x <- p; xs <- many p; return (x:xs)} `orelse` return []

alphanum :: Parser Char
alphanum = sat isAlphaNum

-- use in calculator
letter :: Parser Char
letter = sat isAlpha

ident :: Parser String
ident =  do {c <- lower; cs <- many alphanum; return (c:cs)}

some :: Parser a -> Parser [a]
some p = do {x <- p; xs <- many p; return (x:xs)}

nat :: Parser Int
nat = do {ds <- some digit; return (foldl1 oplus ds)}
         where oplus m n = 10 * m + n

int :: Parser Int
-- int = do {char '-'; n <- nat; return (-n)} `orelse` nat
int = do {f <- op; n <- nat; return (f n)}
         where op = do {char '-'; return negate} `orelse` return id


ints :: Parser[Int]
ints = do char '['
          n <- int
          ns <- many(do{char ','; int})
          char ']'
          return (n:ns)

somewith :: Parser b -> Parser a -> Parser[a]
somewith q p = do x <- p
                  xs <- many(do {q;p})
                  return (x:xs)

manywith :: Parser b -> Parser a -> Parser[a]
manywith q p = somewith q p `orelse` return []



space :: Parser()
space = do {many (sat isSpace); return()}
-- space = many(sat isSpace) >>= return()

token :: Parser a -> Parser a
token p = do {space; x <- p; space; return x}

-- usage: apply (token lowers) "   isLower  "

symbol :: String -> Parser()
symbol xs = token(string xs)



{-
-- expr ::= const | '(' expr addop expr ')'
-- addop ::= '+' | '-'
data Expr = Con Int | Bin Op Expr Expr
data Op = Plus | Minus


expr :: Parser Expr
expr = token const' `orelse`
       do {symbol "("; t <- term; symbol ")"; return t}
const' = do {n <- int; return (Con n)}
term = do {t <- expr; op <- addop; u <- expr; return (Bin op t u)}
addop = do {symbol "+"; return Plus} `orelse`
           do {symbol "-"; return Minus}
-}


{- better
-- expr ::= expr addop factor | factor
-- factor ::= const | '(' expr ')'


expr = term `orelse` factor
term = do {t <- expr; op <- addop; u <- factor; return(Bin op t u) }
factor = token const' `orelse`
         do {symbol "("; e <- expr; symbol ")"; return e}
-}

-- expr = factor `orelse`



-- best
-- expr ::= factor rest
-- rest = addop factor rest | nothing
data Expr = Con Int | Bin Op Expr Expr
data Op = Plus | Minus
expr :: Parser Expr
const' = do {n <- int; return (Con n)}
addop = do {symbol "+"; return Plus} `orelse`
           do {symbol "-"; return Minus}
factor = token const' `orelse`
         do {symbol "("; e <- expr; symbol ")"; return e}
expr = do {t <- factor; rest' t}
rest' t = do {op <- addop; u <- factor; rest' (Bin op t u)}
            `orelse` return t

instance Show Expr where
    show (Con i) = show i
    show (Bin op t u) = "(" ++ show t ++ show op ++ show u ++ ")"
instance Show Op where
    show (Plus) = "+"
    show (Minus) = "-"




as = many (char 'a')
-- usage: apply as ('a' : 'a' : 'b' : undefined)

force :: Parser a -> Parser a
force p = MkP f
          where f cs = (fst (head rs), snd (head rs)):tail rs
                       where rs = apply p cs

many' p = force (some p `orelse` return[])
some' p = do {x <- p; xs <- many p; return(x:xs)}
as' = many' (char 'a')
-- usage: apply as' ('a' : 'a' : 'b' : undefined)





limit :: Parser a -> Parser a
limit p = MkP(first . apply p)
first :: [a] -> [a]
first [] = []
first (r:rs) = [r]

orelse' p q = limit(p `plus` q)



-- main :: IO()
-- main = putStr "hoge"
