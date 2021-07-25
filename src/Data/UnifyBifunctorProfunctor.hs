{-# LANGUAGE FlexibleInstances #-}

module Data.UnifyBifunctorProfunctor (
  FunctorRight,
  FunctorLeft,
  ContravariantLeft,
  Bifunctor,
  Profunctor
  )
  where

-- FunctorRight is a functor on 2nd type parameter for type with 2 type parameters
-- Laws
-- @'rmap' 'id' ≡ 'id'@
-- @'rmap' (f '.' g) ≡ 'map' g '.' 'map' f@
class FunctorRight f where
  rmap :: (b -> bb) -> f a b -> f a bb

-- FunctorLeft is a functor on 1st type parameter for type with 2 type parameters
-- Laws
-- @'lmap' 'id' ≡ 'id'@
-- @'lmap' (f '.' g) ≡ 'lmap' g '.' 'lmap' f@
class FunctorLeft f where
  lmap :: (a -> aa) -> f a b -> f aa b

-- Bifunctor = FunctorLeft + FunctorRight

-- Laws
-- @'bimap' 'id' 'id' ≡ 'id'@
-- @'bimap' (f '.' g) (h '.' i) ≡ 'dimap' g h '.' 'dimap' f i@
class (FunctorLeft f, FunctorRight f) => Bifunctor f where
  bimap :: (a -> aa) -> (b -> bb) -> f a b -> f aa bb
  bimap f g = lmap f . rmap g

-- Laws
-- @'lcontramap' 'id' ≡ 'id'@
-- @'lcontramap' (f '.' g) ≡ 'lcontramap' f '.' 'lcontramap' g@
class ContravariantLeft f where
  lcontramap :: (aa -> a) -> f a b -> f aa b

-- Profunctor = ContravariantLeft + FunctorRight
-- @'dimap' 'id' 'id' ≡ 'id'@
-- @'dimap' (f '.' g) (h '.' i) ≡ 'dimap' g i '.' 'dimap' f h@
class (ContravariantLeft p, FunctorRight p) => Profunctor p where
  dimap :: (aa -> a) -> (b -> bb) -> p a b -> p aa bb
  dimap f g = lcontramap f . rmap g
