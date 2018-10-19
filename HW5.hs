{-# LANGUAGE InstanceSigs #-}

module HW5 where

import Control.Monad
import System.Random
import Test.QuickCheck


{- 5.1 -- Functor instances -}

{-
Write a Functor instance for the following type (which is
pretty much useless outside of its educational value). Let
the types guide you for this problem.

Remember that Functor is the typeclass for types for which
we can provide a sensible definition of `fmap` a.k.a. things
that are "mappable."
-}

data WeirdType a b
  = Foo a
  | Bar b
  | Baz a (WeirdType a b)
  | Buz
  | Biz b (WeirdType a a)

instance Functor (WeirdType a) where
  fmap :: (b -> c) -> WeirdType a b -> WeirdType a c
  fmap = undefined



{- 5.2 -- Identity monad -}

{-
Now it's time for you to define your first Monad instance. `Identity` is
a type that can be used to wrap any value. At first glance it does not
look that interesting. But the Identity monad plays a very crucial
role in Haskell. In particular, it is useful when working with
monad transformers (something we might cover as an advanced topic
later in the semester).
-}

newtype Identity a = Id a

{-
As a reminder, in class our name for `Monad` was `Context` and our names
for `return` and `>>=` were `inject` and `andThen`. If you find it
helpful, instead of defining `>>=` directly you can define
`lift :: (a -> b) -> Identity a -> Identity b` and
`join :: Identity (Identity a) -> Identity a` and then define `>>=`
using the identity `m >>= arr == join (lift arr m)`.
-}

instance Monad Identity where
  return :: a -> Identity a
  return = undefined

  (>>=) :: Identity a -> (a -> Identity b) -> Identity b
  (>>=) = undefined

{-
As of GHC 7.10, Applicative is a superclass of Monad (and Functor
is a superclass of Applicative) which means that we need to also
create instances of these typeclasses for the Identity type.
Luckily, these definitions are made trivial thanks to some
Monad functions. You can look at the types of `ap` and `liftM`
and try playing around with them in GHCi.

Basically, don't worry about this part so much. Whenever you define
your own Monad instance it will be sufficient to copy and paste the
code below and replace Identity with whatever type constructor you
happen to be working with.
-}

instance Applicative Identity where
  pure = return
  (<*>) = ap

instance Functor Identity where
  fmap = liftM


{- 5.3 -- Monadic polymorphism -}

{-
Implement `bothJust` which takes in two `Maybe`s and returns `Just`
of a tuple of the values contained in each if they are both `Just`.
If either is `Nothing` then return `Nothing`.
-}

bothJust :: Maybe a -> Maybe b -> Maybe (a, b)
bothJust = undefined

{-
Implement `allPairs` which takes in two lists and returns a list
of all pairs of elements from these lists. In other words,
`allPairs` computes the cartesian product of the lists.
-}

allPairs :: [a] -> [b] -> [(a, b)]
allPairs = undefined

{-
Look at the type signatures of `bothJust` and `allPairs`. They look
similar, no? The only difference between them is the type constructor
(i.e. the container) wrapping the values-- the type constructor in
`bothJust` is `Maybe` and the type constructor in `allPairs` is `[]`.
Furthermore , `Maybe` and `[]` are both monadic, which suggests that
using the proper `Monad` abstactions we can perhaps replace the two
functions above with a single polymorphic function that has the
same behavior.
-}

liftTuple :: Monad m => m a -> m b -> m (a, b)
liftTuple = undefined

{- TO BE CONTINUED... -}
