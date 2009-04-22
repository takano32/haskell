#!/usr/bin/env hugs
import List
import Parser
-- standard function
singleton :: [a] -> Bool
singleton xs = not (null xs) && null (tail xs)

-- http://uni.m0nkey.de/info/pdfs/haskell/konstanz/ueb-8.lhs
-- joinWith :: a -> [[a]] -> [a]
-- joinWith g = concat . intersperse [g]
-- concat (intersperse "h" ["Hello", "World"])
joinWith :: [a] -> [[a]] -> [a]
joinWith g = concat . intersperse g



-- 12.1 Basic considerations



pairs
    = map parseLaw [
      "definition fst:      fst.pair(f, g) = f",
      "definition snd:      snd.pair(f, g) = g",
      "definition cross:    cross(f, g) = pair(f.fst, g.snd)",
      "pair absorption:     pair(f, g).h = pair(f.h, g.h)"]

simplify :: [Law] -> String -> IO()
simplify laws = putStr . printCalc . calculate (laws,others) . parseExpr

-- usage: simplify pairs "cross(f, g).pair(h, k)"

-- 12.1.1 Proofs

laws = filters ++ ifs ++ others

filters
    = map parseLaw [
      "definition filter: filter p = concat.map(box p)",
      "definition box:    box p = if(p, wrap, nil)"]

ifs
    = map parseLaw [
      "if over composition: if(p,f,g).h = if(p.h, f.h, g.h)",
      "composition over if: h.if(p,f,g) = if(p, h.f, h.g)"]

others
    = map parseLaw [
      "nil constant:       nil.f = nil",
      "nil natural:        map f.nil = nil",
      "wrap natural:       map f.wrap = wrap.f",
      "concat natural:     map f.concat = concat.map(map f)",
      "map functor:        map f.map g = map(f.g)"]

-- uage: prove laws "filter p.map f = map f.filter(p.f)"


-- calculate :: [Law] -> Expr -> Calculation
prove :: [Law] -> String -> IO()
prove laws = putStr . printCalc . proveEqn laws . parseEqn

proveEqn :: [Law] -> (Expr, Expr) -> Calculation
proveEqn laws (lhs, rhs)
    = paste (calculate (laws,others) lhs) (calculate (laws,others) rhs)

-- 12.2 Expressions, laws, and calculations
data Expr
    = Var VarName | Con ConName[Expr] | Compose[Expr]
      deriving(Eq)

type VarName = Char
type ConName = String

compose :: [Expr] -> Expr
compose xs = if singleton xs
             then head xs
             else Compose(concat(map decompose xs))

decompose :: Expr -> [Expr]
decompose (Var v) = [Var v]
decompose (Con f xs) = [Con f xs]
decompose (Compose xs) = xs

complexity :: Expr -> Int
complexity (Var v) = 1
complexity (Con f xs) = 1
complexity (Compose xs) = length xs

printExpr :: Expr -> String
printExpr (Var v) = [v]

printExpr (Con f xs)
    | null xs = f
    | simple xs = f ++ " " ++ printExpr (head xs)
    | otherwise = f ++ "(" ++
                  joinWith ", " (map printExpr xs) ++ ")"

printExpr (Compose xs) = joinWith "." (map printExpr xs)

simple :: [Expr] -> Bool
simple xs = singleton xs && simpleton (head xs)

simpleton :: Expr -> Bool
simpleton (Var v) = True
simpleton (Con f xs) = null xs
simpleton (Compose xs) = False

-- 12.2.1

parseExpr :: String -> Expr
parseExpr = applyParser expr

expr :: Parser Expr
expr = do {xs <- somewith (symbol ".") term; return(compose xs)}

term :: Parser Expr
term = do space
          c <- letter
          cs <- many alphanum
          if null cs
             then return (Var c)
             else do xs <- argument
                     return (Con(c:cs) xs)
argument :: Parser [Expr]
argument = tuple `orelse` (notuple `orelse` return [])

tuple :: Parser [Expr]
tuple = do symbol "("
           xs <- somewith (symbol ",") expr
           symbol ")"
           return xs

notuple :: Parser [Expr]
notuple = do space
             c <- letter
             cs <- many alphanum
             if null cs
               then return [Var c]
               else return [Con(c:cs) []]

parseEqn :: String -> (Expr, Expr)
parseEqn = applyParser eqn
eqn :: Parser (Expr, Expr)
eqn = do space
         x <- expr
         symbol "="
         y <- expr
         return (x,y)

-- 1.2.2.2 Laws

type Law = (LawName, Expr, Expr)
type LawName = String


basicLaw :: Law -> Bool
basicLaw (name, lhs, rhs) = (complexity lhs > complexity rhs)

parseLaw :: String -> Law
parseLaw = applyParser law

law :: Parser Law
law = do space
         name <- some (sat (/= ':'))
         symbol ":"
         (x,y) <- eqn
         return (name, x, y)

-- 12.2.3 Calculations

type Calculation = (Expr, [Step])
type Step = (LawName, Expr)

conclusion :: Calculation -> Expr
conclusion(x, steps) = if null steps then x else snd (last steps)

paste :: Calculation -> Calculation -> Calculation
paste lhc rhc = (fst lhc, snd lhc ++ link x y ++ shuffle rhc)
                where x = conclusion lhc
                      y = conclusion rhc

link :: Expr -> Expr -> [Step]
link x y = if x == y then [] else [("... ??? ...", y)]

shuffle :: Calculation -> [Step]
shuffle(x,ss) = snd(foldl shunt (x, []) ss)
                where shunt (x, rs) (r, y) = (y, (r, x):rs)

printCalc :: Calculation -> String
printCalc (x,ss) = "\n  " ++ printExpr x ++
                   "\n" ++ concat (map printStep ss)

printStep :: Step -> String
printStep (why, x) = "=  {" ++ why ++ "}\n" ++
                     "  " ++ printExpr x ++ "\n"




-- 12.3.1 Substitutions

type Subst = [(VarName, Expr)]

binding :: Subst -> VarName -> Expr
binding [] v = Var v
binding ((u, x) :s)v = if u == v then x else binding s v

applySub :: Subst -> Expr -> Expr
applySub s (Var v) = binding s v
applySub s (Con f xs) = Con f (map (applySub s) xs)
applySub s (Compose xs) = compose (map (applySub s)xs)

extend :: Subst -> (VarName, Expr) -> [Subst]
extend s (v, x)
    | y == x = [s]
    | y == Var v = [(v, x) : s]
    | otherwise = []
    where y = binding s v


-- 12.3.2 Matching
match :: (Expr, Expr) -> [Subst]
match (x, y) = xmatch[] (x, y)

xmatch :: Subst -> (Expr, Expr) -> [Subst]
xmatch s (Var v, x) = extend s (v, x)
xmatch s (Con f xs, Var v) = []
xmatch s (Con f xs, Compose ys) = []
xmatch s (Con f xs, Con g ys)
    = if f == g then xmatchlist s (zip xs ys) else []


xmatch s (Compose xs, Var v) = []
xmatch s (Compose xs, Con g ys) = []
xmatch s (Compose xs, Compose ys)
    = concat (map (xmatchlist s)(align xs ys))

align :: [Expr] -> [Expr] -> [[(Expr, Expr)]]
align xs ys = [zip xs (map compose zs)| zs <- parts (length xs)ys]

parts :: Int -> [a] -> [[[a]]]
parts 0 [] = [[]]
parts 0 (x:xs) = []
parts (n+1) [] = []
parts (n+1) (x:xs) = map (new x) (parts n xs) ++
                     map (glue x) (parts (n+1) xs)

new :: a -> [[a]] -> [[a]]
new x yss = [x] : yss
glue :: a -> [[a]] -> [[a]]
glue x (ys:yss) = (x:ys) : yss

xmatchlist :: Subst -> [(Expr, Expr)] -> [Subst]
xmatchlist s [] = [s]
xmatchlist s (xy:xys) = concat [xmatchlist t xys | t <- xmatch s xy]



-- 12.4 Subexpressions and rewriteing

type SubExpr = (Location, Expr)

data Location = All | Seg Int Int | Pos Int Location


subexprs :: Expr -> [SubExpr]
subexprs (Var v) = [(All, Var v)]
subexprs (Con f xs) = [(All, Con f xs)] ++ subterms xs
subexprs (Compose xs) = [(All, Compose xs)] 
                        ++ segments xs ++ subterms xs

subterms :: [Expr] -> [SubExpr]
subterms xs
    = [(Pos j loc, y)|j <- [0..n-1], (loc, y) <- subexprs(xs!!j)]
      where n = length xs


segments :: [Expr] -> [SubExpr]
segments xs 
    = [(Seg j k, Compose (take k (drop j xs)))
           | k <- [2..n-1], j <- [0..n-k]]
      where n = length xs

replace :: Expr -> Location -> Expr -> Expr
replace x All y = y

replace (Con f xs) (Pos j loc) y
    = Con f (take j xs ++ [replace (xs !! j) loc y] ++ drop (j+1) xs)
replace (Compose xs) (Pos j loc) y
    = compose (take j xs ++ [replace (xs !! j) loc y] ++ drop (j+1) xs)
replace (Compose xs) (Seg j k)y
    = compose (take j xs ++ [y] ++ drop (j+k) xs)

-- 12.4.1 Rewriting

calculate :: ([Law], [Law]) -> Expr -> Calculation
calculate pls x = (x, repeatedly (rewrites pls) x)


rewrites :: ([Law], [Law]) -> Expr -> [Step]
rewrites (llaws, rlaws) x
    = concat ([rewrite law sx x|law <- llaws, sx <- subexprs x]
              ++ [rewrite law sx x|sx <- subexprs x, law <- rlaws])

rewrite :: Law -> SubExpr -> Expr -> [Step]
rewrite (name, lhs, rhs) (loc, y) x
    = [(name, replace x loc (applySub s rhs))|s <- match(lhs, y)]

repeatedly :: (Expr -> [Step]) -> Expr -> [Step]
repeatedly rws x
    = if null steps then [] else (n, y) : repeatedly rws y
      where steps = rws x
            (n, y) = head steps

-- 12.5 Testing the Calculator

cplist :: [[a]] -> [[a]]
cplist [] = [[]]
cplist (xs:xss) = [y:ys|y <- xs, ys <- cplist xss]

unify :: [Subst] -> [Subst]
unify [] = [[]]
unify (s:ss) = concat [union' s t|t <- unify ss]


union' :: Subst -> Subst -> [Subst]
union' s t = if compatible s t then [merge s t] else []
             where compatible s t = True
                   merge = (++)

cup :: [Subst] -> [Subst] -> [Subst]
cup ss ts = concat [union' s t|s <- ss, t <- ts]

--
--
-- 12.5.2 The combinators

star :: (a -> [b]) -> [a] -> [b]
star f = concat . map f

cpp :: ([a], [b]) -> [(a, b)]
cpp (xs, ys) = [(x, y)|x <- xs, y <- ys]

cpr :: (a, [b]) -> [(a, b)]
cpr (x, ys) = [(x, y)| y <- ys]

cpl :: ([a], b) -> [(a, b)]
cpl (xs, y) = [(x, y)|x <- xs]

assl :: (a, (b, c)) -> ((a, b), c)
assl (x, (y, z)) = ((x, y), z)

matchs
    = map parseLaw [
      "def matchlist: matchlist = star unify.cplist.map match",
      "def unify: unify.nil = wrap.nil",
      "def unify: unify.cons = star union.cpr.cross(id,unify)",
      "def cplist: cplist.nil = wrap.nil",
      "cef cplist: cplist.cons = map cons.cpp.cross(id,cplist)"]


gmatchs
    = map parseLaw [
      "def gmatchlist: gmatchlist = star union.cpp.cross(id,matchlist)",
      "def gmatch:     gmatch     = star union.cpp.cross(id,match)"]

xmatchs
    = map parseLaw [
      "def xmatchlist: xmatchlist = gmatchlist.cross(wrap,id)",
      "def xmatch:     xmatch     = gmatch.cross(wrap,id)"]

laws1 = stars ++ cps ++ ids ++ functors

stars
    = map parseLaw[
      "star after map:    star f.map g = star(f.g)",
      "star after concat: star f.concat = star(star f)",
      "star after wrap:   star f.wrap = f",
      "star after nil:    star f.nil = nil",
      "star after star:   star(star f.g) = star f.star g"]

cps = cpps ++ cpls ++ cprs

cpps
    = map parseLaw[
      "def cpp:          cpp = star cpr.cpl"]
cpls
    = map parseLaw[
      "cpl after nil:    cpl.cross(nil,id) = nil",
      "cpl after wrap:   cpl.cross(wrap,id) = wrap",
      "cpl after map:    cpl.cross(map f,id) = map (cross(f,id)).cpl",
      "cpl after concat: cpl.cross(concat,id) = star cpl.cpl",
      "cpl after id:     cpl.cross(id,g) = map(cross(id,g)).cpl",
      "cpl after star:   cpl.cross(star f,g) = star(cpl.cross(f,g)).cpl"]
cprs
    = map parseLaw[
      "cpr after nil:    cpr.cross(id,nil) = nil",
      "cpr after wrap:   cpr.cross(id,wrap) = wrap",
      "cpr after map:    cpr.cross(id,map g) = map(cross(id,g)).cpr",
      "cpr after concat: cpr.cross(id,concat) = star cpr.cpr",
      "cpr after id:     cpr.cross(f,id) = map(cross(f,id)).cpr",
      "cpr after star:   cpr.cross(f,star g) = star(cpr.cross(f,g)).cpr"]

ids
    = map parseLaw[
      "id left unit:     id.f = f",
      "id right unit:    f.id = f"]

functors
    = map parseLaw[
      "cross functor:  cross(f,g).cross(h,k) = cross(f.h, g.k)",
      "cross functor:  cross(id,id) = id",
      "map fanctor:    map f.map g = map(f.g)",
      "map functor:    map id = id",
      "map after nil:  map f.nil = nil",
      "map after cons: map f.cons = cons.cross(f,map f)"]
claims
    = map parseLaw [
      "claim: star(cpr.cross(id,g)).cpp = cpp.cross(id,star g)"]


-- usage: prove laws1 "star(cpr.cross(id,g)).cpp = cpp.cross(id,star g)"


laws2 = matchs ++ laws1 ++ claims
-- usage: prove laws2 "matchlist.nil = wrap.nil"
-- usage: prove laws2 "matchlist.cons = star union.cpp.cross(match,matchlist)"
