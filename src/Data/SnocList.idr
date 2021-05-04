||| A Reversed List
module Data.SnocList

import Decidable.Equality
import Data.List

%default total

public export
data SnocList : (type : Type) -> Type where
  Empty : SnocList type
  Snoc  : SnocList type -> type -> SnocList type

%name SnocList sx, sy, sz

||| Reverse the list and make a list.
public export
toList : SnocList type -> List type
toList Empty = Nil
toList (Snoc sx x) = x :: toList sx

||| Transform to a list but keeping the contents in the 'correct' order.
public export
asList : SnocList type -> List type
asList = (reverse . toList)


public export
Eq a => Eq (SnocList a) where
  (==) Empty Empty = True
  (==) (Snoc sx x) (Snoc sy y) = x == y && sx == sy
  (==) _ _ = False


public export
Ord a => Ord (SnocList a) where
  compare Empty Empty = EQ
  compare Empty (Snoc sx x) = LT
  compare (Snoc sx x) Empty = GT
  compare (Snoc sx x) (Snoc sy y)
    = case compare sx sy of
        EQ => compare x y
        c  => c

public export
(++) : (sx, sy : SnocList a) -> SnocList a
(++) sx Empty = sx
(++) sx (Snoc sy y) = Snoc (sx ++ sy) y

public export
length : SnocList a -> Nat
length Empty = Z
length (Snoc sx x) = length sx + 1

public export
Functor SnocList where
  map f Empty = Empty
  map f (Snoc sx x) = Snoc (map f sx) (f x)


public export
Semigroup (SnocList a) where
  (<+>) = (++)

public export
Monoid (SnocList a) where
  neutral = Empty


||| Check if something is a member of a list using the default Boolean equality.
public export
elem : Eq a => a -> SnocList a -> Bool
elem x Empty = False
elem x (Snoc sx y) = x == y || elem x sx


-- [ EOF ]
