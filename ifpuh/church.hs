#!/usr/bin/env ruby
hoge = \a b c -> a+b+c

zero = \f x -> x
succ n = \f x -> f (n f x)




-- zero = \f (\x -> x) -> 
{-
(lambda (f) (lambda (x) x))
-}


