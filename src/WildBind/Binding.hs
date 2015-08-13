-- |
-- Module: WildBind.Binding
-- Description: Functions to build Binding
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
module WildBind.Binding (
  -- * Types
  Action(Action,actDescription,actDo),
  Binding,
  -- * Execution
  boundAction,
  boundInputs,
  -- * Construction
  stateless,
  rawBinds,
  on',
  -- * Condition
  whenS,
  -- * Conversion
  contramapFrontState,
  mapInput,
  invmapBackState,
  -- * Explicitly Stateful Bindings
  Binding',
  boundAction',
  boundInputs',
  startFrom,
  stateIgnored
) where

import qualified Data.Map as M
import Data.Monoid (Monoid(..))

import WildBind.Internal.FrontEnd (ActionDescription)

-- | Action done by WildBind
data Action m a = Action {
  actDescription :: ActionDescription, -- ^ Human-readable description of the action.
  actDo :: m a -- ^ The actual job.
}

instance Functor m => Functor (Action m) where
  fmap f a = a { actDo = fmap f (actDo a) }

-- | WildBind back-end binding with both explicit and implicit
-- states. @bs@ is the explicit back-end state, @fs@ is the front-end
-- state, and @i@ is the input type.
newtype Binding' bs fs i = Binding' {
  unBinding' :: bs -> fs -> M.Map i (Action IO (Binding' bs fs i, bs))
}

-- | WildBind back-end binding between inputs and actions. @s@ is the
-- front-end state type, and @i@ is the input type.
type Binding s i = Binding' () s i

-- | 'mempty' returns a 'Binding' where no binding is
-- defined. 'mappend' combines two 'Binding's while preserving their
-- individual implicit states. The right-hand 'Binding' has precedence
-- over the left-hand one. That is, if the two 'Binding's both have a
-- binding to the same key in the same front-end and back-end state,
-- the binding from the right-hand one is used.
instance Ord i => Monoid (Binding' bs fs i) where
  mempty = Binding' $ \_ _ -> M.empty
  mappend abind bbind = Binding' $ \bs fs ->
    let amap = mapResult (`mappend` bbind) id $ unBinding' abind bs fs
        bmap = mapResult (abind `mappend`) id $ unBinding' bbind bs fs
    in M.unionWith (\_ b -> b) amap bmap
    

-- | Get the 'Action' bound to the specified state @s@ and input @i@.
boundAction :: (Ord i) => Binding s i -> s -> i -> Maybe (Action IO (Binding s i))
boundAction binding state input = (fmap . fmap) fst $ boundAction' binding () state input

-- | Get the 'Action' bound to the specified back-end state @bs@,
-- front-end state @fs@ and input @i@
boundAction' :: (Ord i) => Binding' bs fs i -> bs -> fs -> i -> Maybe (Action IO (Binding' bs fs i, bs))
boundAction' binding bs fs input = M.lookup input $ unBinding' binding bs fs

-- | Get the list of all inputs @i@ bound to the specified state @s@.
boundInputs :: Binding s i -> s -> [i]
boundInputs binding state = boundInputs' binding () state

-- | Get the list of all inputs @i@ bound to the specified back-end
-- state @bs@ and front-end state @fs@.
boundInputs' :: Binding' bs fs i -> bs -> fs -> [i]
boundInputs' binding bs fs = M.keys $ unBinding' binding bs fs

-- | Build a 'Binding' with no explicit or implicit state.
stateless :: Ord i
          => [(i, Action IO ())] -- ^ Bound pairs of input symbols and 'Action'.
          -> Binding s i
          -- ^ Result Binding. The given bindings are activated
          -- regardless of the front-end state.
stateless blist = rawBinds $ fmap (mapSnd $ fmap (const $ stateless blist)) blist
  where mapSnd f (a,b) = (a, f b)

-- | Build a 'Binding' from a list. This is a raw-level function to
-- build a 'Binding'. 'stateless' and 'stateful' are recommended.
rawBinds :: Ord i
         => [(i, Action IO (Binding s i))] -- ^ Bound pairs of input symbol and 'Action'
         -> Binding s i
         -- ^ Result Binding. The given bindings are activated
         -- regardless of the front-end state.
rawBinds blist = Binding' $ \_ _ ->
  (fmap . fmap) (\b -> (b, ())) $ M.fromList blist

-- | Build a single pair of binding.
on' :: i -> ActionDescription -> m a -> (i, Action m a)
on' input desc act = (input, Action { actDescription = desc, actDo = act })

-- | Add a condition to 'Binding'.
whenS :: (fs -> Bool) -- ^ Condition about the front-end state.
      -> Binding' bs fs i -- ^ Original Binding.
      -> Binding' bs fs i -- ^ Result Binding where the original
                          -- Binding is activated only when the given
                          -- condition is 'True'.
whenS cond orig_bind = Binding' $ \bs fs ->
  if cond fs
  then unBinding' orig_bind bs fs
  else M.empty

-- infixr version of 'whenS' may be useful, too.

mapResult :: Functor m => (a -> a') -> (b -> b') -> M.Map i (Action m (a, b)) -> M.Map i (Action m (a',b'))
mapResult amapper bmapper = (fmap . fmap) (\(a, b) -> (amapper a, bmapper b))

-- | Contramap the front-end state.
contramapFrontState :: (fs -> fs') -> Binding' bs fs' i -> Binding' bs fs i
contramapFrontState cmapper orig_bind = Binding' $ \bs fs ->
  mapResult (contramapFrontState cmapper) id $ unBinding' orig_bind bs (cmapper fs)

-- | Map the front-end input.
mapInput :: Ord i' => (i -> i') -> Binding' bs fs i -> Binding' bs fs i'
mapInput mapper orig_bind = Binding' $ \bs fs ->
  mapResult (mapInput mapper) id $ M.mapKeys mapper $ unBinding' orig_bind bs fs

-- | Invariant-map the back-end state.
invmapBackState :: (bs -> bs') -> (bs' -> bs) -> Binding' bs fs i -> Binding' bs' fs i
invmapBackState mapper cmapper orig_bind = Binding' $ \bs fs ->
  mapResult (invmapBackState mapper cmapper) mapper $ unBinding' orig_bind (cmapper bs) fs

-- | Convert 'Binding'' to 'Binding' by hiding the explicit state
-- @bs@.
startFrom :: bs -- ^ Initial state
          -> Binding' bs fs i -- ^ Binding' with explicit state
          -> Binding fs i -- ^ Binding containing the state inside
startFrom init_state b' = Binding' $ \() front_state ->
  (fmap . fmap) toB $ unBinding' b' init_state front_state
  where
    toB (next_b', next_state) = (startFrom next_state next_b', ())

-- | Extend 'Binding' to 'Binding''. The explicit back-end state is
-- unmodified.
stateIgnored :: Binding fs i -> Binding' bs fs i
stateIgnored b = Binding' $ \bs fs ->
  mapResult stateIgnored (const bs) $ unBinding' b () fs
