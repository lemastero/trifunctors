{-# LANGUAGE FlexibleInstances #-}

module Data.Zifunctor (
  FunctorLeft(..),
  FunctorRight(..),
  ContravariantRight(..),
  Joker,
  Clown,
  Bard,
  Zifunctor(..),
  CurriedRev,
  ContravariantLeft,
  Fool,
  Nifunctor,
  Trifunctor(..)
  )
  where

-- FunctorRight is a functor on third type parameter for type with 3 type parameters
-- Laws
-- @'rmap' 'id' ≡ 'id'@
-- @'rmap' (f '.' g) ≡ 'map' g '.' 'map' f@
class FunctorRight f where
  rmap :: (r -> rr) -> f e a r -> f e a rr

-- Makes a Functor over a third argument of type with 3 type parameters
newtype Joker f e a r = Joker { runJoker :: f r }
  deriving ( Eq, Ord )

-- FunctorLeft is a functor on second type parameter for type with 3 type parameters
-- Laws
-- @'lmap' 'id' ≡ 'id'@
-- @'lmap' (f '.' g) ≡ 'lmap' g '.' 'lmap' f@
class FunctorLeft f where
  lmap :: (a -> aa) -> f e a r -> f e aa r

newtype Clown f e a r = Clown { runClown :: f a }
  deriving ( Eq, Ord )

-- ContravariantRight is a contravariant functor on first type parameter for type with 3 type parameters
-- Laws
-- @'rcontramap' 'id' ≡ 'id'@
-- @'rcontramap' (f '.' g) ≡ 'rcontramap' f '.' 'rcontramap' g@
class ContravariantRight f where
  rcontramap :: (ee -> e) -> f e a r -> f ee a r

newtype Bard f e a r = Bard { runBard :: f e }
  deriving ( Eq, Ord )

-- Zifunctor type with 3 type parameters where first is contravariant and rest is covariant
-- canonical examples:
--   e -> Either a r
--   e -> IO (Either a r)
--   e -> (a, r)
--   e -> IO (a, r)
--   (a -> e) -> r
--   e -> Either [a] r
--   e -> ([a], r)

-- Laws
-- @'zimap' 'id' 'id' 'id' ≡ 'id'@
-- @'zimap' (f '.' g) (h '.' i) (j '.' k) ≡ 'zimap' g h j '.' 'zimap' f i k@
class (FunctorLeft f, FunctorRight f, ContravariantRight f) => Zifunctor f where
  zimap :: (ee -> e) -> (a -> aa) -> (r -> rr) -> f e a r -> f ee aa rr
  zimap f g h = rmap h . lmap g . rcontramap f

  bimap :: (a -> aa) -> (r -> rr) -> f e a r -> f e aa rr
  bimap = zimap id

  dimapLeft :: (ee -> e) -> (a -> aa) -> f e a r -> f ee aa r
  dimapLeft f g = zimap f g id

  dimap :: (ee -> e) -> (r -> rr) -> f e a r -> f ee a rr
  dimap f = zimap f id

newtype CurriedRev b c a = CurriedRev { run :: (c -> b) -> a }

instance FunctorRight CurriedRev where
  rmap h (CurriedRev fa) = CurriedRev (h . fa)

instance ContravariantRight CurriedRev where
  rcontramap f (CurriedRev fa) = CurriedRev (\k -> fa (f . k))

instance FunctorLeft CurriedRev where
  lmap g (CurriedRev fa) = CurriedRev (\k -> fa (k . g))

instance Zifunctor CurriedRev where
  zimap f g h (CurriedRev fa) = CurriedRev (\k -> h (fa (f . k . g)))

-- TODO instance a -> Either b c
-- TODO instance a -> (b, c)
-- TODO instance a -> IO (b, c)
-- TODO instance Optics Get

---------------
-- NIFUNCTOR --
---------------

-- Laws
-- @'lcontramap' 'id' ≡ 'id'@
-- @'lcontramap' (f '.' g) ≡ 'lcontramap' f '.' 'lcontramap' g@
class ContravariantLeft f where
  lcontramap :: (aa -> a) -> f e a r -> f e aa r

newtype Fool f e a r = Fool { runFool :: f e }
  deriving ( Eq, Ord )

-- Nifunctor type with 3 type parameters where first and second is contravariant and last is covariant
-- canonical examples:
--   Either e a -> r
--   Either e a -> IO r
--   e -> a -> r
--   e -> a -> IO r
--   (e, a) -> r
-- @'nimap' 'id' 'id' 'id' ≡ 'id'@
-- @'nimap' (f '.' g) (h '.' i) (j '.' k) ≡ 'dimap' g i j '.' 'dimap' f h k@
class (FunctorRight f, ContravariantLeft f, ContravariantRight f) => Nifunctor f where
  nimap :: (ee -> e) -> (aa -> a) -> (r -> rr) -> f e a r -> f ee aa rr
  nimap f g h = rmap h . lcontramap g . rcontramap f

newtype Function2 b c a = Function2 { runFun2 :: b -> c -> a }

instance FunctorRight Function2 where
  rmap h (Function2 fa) = Function2 ( \ee -> h . fa ee )

instance ContravariantLeft Function2 where
  lcontramap g (Function2 fa) = Function2 ( \ee -> fa ee . g )

instance ContravariantRight Function2 where
  rcontramap f (Function2 fa) = Function2 ( fa . f )

instance Nifunctor Function2 where
  nimap f g h (Function2 fa) = Function2 ( \ee -> h . fa (f ee) . g )

-- TODO instance Either a b -> c
-- TODO instance (a, b) -> c
-- TODO instance Optics Set

----------------
-- Trifunctor --
----------------

-- Laws
-- @'fstMap' 'id' ≡ 'id'@
-- @'fstMap' (f '.' g) ≡ 'fstMap' f '.' 'fstMap' g@
class FstFunctor f where
  fstmap :: (e -> ee) -> f e a r -> f ee a r

class (FstFunctor f, FunctorLeft f, FunctorRight f) => Trifunctor f where
  trimap :: (e -> ee) -> (a -> aa) -> (r -> rr) -> f e a r -> f ee aa rr
  trimap f g h = rmap h . lmap g . fstmap f

instance FunctorLeft (,,) where
  lmap f ~(e, a, r) = (e, f a, r)

instance FunctorRight (,,) where
  rmap f ~(e, a, r) = (e, a, f r)

instance FstFunctor (,,) where
  fstmap f ~(a, b, c) = (f a, b, c)

instance Trifunctor (,,) where
  trimap f g h ~(a, b, c) = (f a, g b, h c)

