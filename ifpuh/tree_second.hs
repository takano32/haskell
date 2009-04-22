#!/usr/bin/env ruby


data Term = Val Char | App Char Term Term

trees :: [Char] -> [Char] -> [Term]
trees ds os = [ t | (_,t) <- [ otrees os u | u <- dtrees ds ] ]

dtrees :: [Char] -> [Term]
dtrees [x] = [Val x]
dtrees ds = concat [joins ls rs | (ls,rs) <- [ lrs xs ys | (xs,ys) <- splits1 ds ] ]

splits1 :: [Char] -> [([Char], [Char])]
splits1 [x] = []
splits1 (x:xs) = ([x],xs) : [ (x:ys,zs) | (ys,zs) <- splits1 xs ]

lrs :: [Char] -> [Char] -> ([Term],[Term])
lrs xs ys = (dtrees xs, dtrees ys)

joins :: [Term] -> [Term] -> [Term]
joins ls rs = [ App '^' l r | l <- ls, r <- rs ]

otrees :: [Char] -> Term -> ([Char], Term)
otrees os (Val c) = (os, Val c)
otrees os (App _ l r) = (os'', App o' l' r')
    where (o':os',l') = otrees os l
          (os'', r') = otrees os' r

instance Show Term where
    show (Val c) = [c]
    show (App o l r) = "(" ++ show l ++ [o] ++ show r ++ ")"



-- Calculate




