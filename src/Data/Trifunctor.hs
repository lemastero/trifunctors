module Data.Trifunctor where

-- Joker is a functor on third type parameter for type with 3 type parameters
-- Laws
-- @'rmap' 'id' ≡ 'id'@
-- @'rmap' (f '.' g) ≡ 'map' g '.' 'map' f@
class Joker f where
  rmap :: (r -> rr) -> f e a r -> f e a rr

-- Joker is a functor on second type parameter for type with 3 type parameters
-- Laws
-- @'lmap' 'id' ≡ 'id'@
-- @'lmap' (f '.' g) ≡ 'lmap' g '.' 'lmap' f@
class Clown f where
  lmap :: (a -> aa) -> f e a r -> f e aa r

-- Bard is a contravariant functor on first type parameter for type with 3 type parameters
-- Laws
-- @'rcontramap' 'id' ≡ 'id'@
-- @'rcontramap' (f '.' g) ≡ 'rcontramap' f '.' 'rcontramap' g@
class Bard f where
  rcontramap :: (ee -> e) -> f e a r -> f ee a r

-- Trifunctor type with 3 type parameters where first is contravariant and rest is covariant
-- canonical examples:
--   e -> Either a r
--   e -> IO (Either a r)
--   e -> (a, r)
--   e -> IO (a, r)
--   (a -> e) -> r
-- Laws
-- @'timap' 'id' 'id' 'id' ≡ 'id'@
-- @'dimap' (f '.' g) (h '.' i) (j '.' k) ≡ 'dimap' g h j '.' 'dimap' f i k@
class (Joker f, Clown f, Bard f) => Trifunctor f where
  timap :: (ee -> e) -> (a -> aa) -> (r -> rr) -> f e a r -> f ee aa rr


-- Laws
-- @'lcontramap' 'id' ≡ 'id'@
-- @'lcontramap' (f '.' g) ≡ 'lcontramap' f '.' 'lcontramap' g@
class Fool f where
  lcontramap :: (aa -> a) -> f e a r -> f e aa r

  -- Nifunctor type with 3 type parameters where first and second is contravariant and last is covariant
  -- canonical examples:
  --   Either e a -> r
  --   Either e a -> IO r
  --   e -> a -> r
  --   e -> a -> IO r
  --   (e, a) -> r
-- @'nimap' 'id' 'id' 'id' ≡ 'id'@
-- @'nimap' (f '.' g) (h '.' i) (j '.' k) ≡ 'dimap' g i j '.' 'dimap' f h k@
class (Joker f, Fool f, Bard f) => Nifunctor f where
  nimap :: (ee -> e) -> (aa -> a) -> (r -> rr) -> f e a r -> f ee aa rr
