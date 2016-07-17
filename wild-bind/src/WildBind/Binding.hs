{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |
-- Module: WildBind.Binding
-- Description: Functions to build Binding
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- This module exports functions to build and manipulate 'Binding', an
-- object binding input symbols to actions.
-- 
module WildBind.Binding
       ( -- * Types
         Action(Action,actDescription,actDo),
         Binding,
         Binding',
         
         -- * Construction

         -- | Functions to create fundamental 'Binding's.
         --
         -- To create complex 'Binding's, use <#Condition Condition> functions
         -- described below and 'mappend' them together.
         
         noBinding,
         Binder,
         binds,
         binds',
         on,
         run,
         as,
         binding,
         binding',
         
         -- * Condition
         
         -- | #Condition# With these functions, you can create
         -- 'Binding's that behave differently for different front-end
         -- and/or back-end states.
         --
         -- If you call the condition functions multiple times, the
         -- conditions are combined with AND logic.
         ifFront,
         ifBack,
         ifBoth,
         whenFront,
         whenBack,
         whenBoth,
         -- * Conversion
         advice,
         before,
         after,
         startFrom,
         extend,
         extendAt,
         convFront,
         convInput,
         convBack,
         -- * Execution
         boundAction,
         boundAction',
         boundActions,
         boundActions',
         boundInputs,
         boundInputs'
       ) where

import Control.Monad.Trans.State (StateT, runStateT)
import Control.Monad.Trans.Writer (Writer, tell, execWriter)
import qualified Data.Map as M
import Data.Monoid (Monoid(..), Endo(Endo, appEndo))
import Lens.Micro ((^.), (.~), (&))
import qualified Lens.Micro as Lens

import WildBind.Description (ActionDescription)

-- | Action done by WildBind
data Action m a =
  Action
  { actDescription :: ActionDescription, -- ^ Human-readable description of the action.
    actDo :: m a -- ^ The actual job.
  }

instance Show (Action m a) where
  show a = "Action " ++ show (actDescription a)

instance Functor m => Functor (Action m) where
  fmap f a = a { actDo = fmap f (actDo a) }

-- | Make an 'Action' that runs the given monadic action before the
-- original 'Action'.
before :: (Monad m)
       => m b -- ^ the monadic action prepended
       -> Action m a -- ^ the original 'Action'.
       -> Action m a
before = undefined

-- | Make an 'Action' that runs the given monadic action after the
-- original 'Action'.
after :: (Monad m)
      => m b -- ^ the monadic action appended.
      -> Action m a -- ^ the original 'Action'.
      -> Action m a
after = undefined

-- | WildBind back-end binding with both explicit and implicit
-- states. @bs@ is the explicit back-end state, @fs@ is the front-end
-- state, and @i@ is the input type.
--
-- You can make the explicit state @bs@ implicit by 'startFrom'
-- function.
newtype Binding' bs fs i =
  Binding'
  { unBinding' :: bs -> fs -> M.Map i (Action IO (Binding' bs fs i, bs))
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
  mempty = noBinding
  mappend abind bbind = Binding' $ \bs fs ->
    let amap = mapResult (`mappend` bbind) id $ unBinding' abind bs fs
        bmap = mapResult (abind `mappend`) id $ unBinding' bbind bs fs
    in M.unionWith (\_ b -> b) amap bmap

-- | A 'Binding'' with no bindings. It's the same as 'mempty', except
-- 'noBinding' requires no context.
noBinding :: Binding' bs fs i
noBinding = Binding' $ \_ _ -> M.empty

-- | Get the 'Action' bound to the specified state @s@ and input @i@.
boundAction :: (Ord i) => Binding s i -> s -> i -> Maybe (Action IO (Binding s i))
boundAction b state input = (fmap . fmap) fst $ boundAction' b () state input

-- | Get the 'Action' bound to the specified back-end state @bs@,
-- front-end state @fs@ and input @i@
boundAction' :: (Ord i) => Binding' bs fs i -> bs -> fs -> i -> Maybe (Action IO (Binding' bs fs i, bs))
boundAction' b bs fs input = M.lookup input $ unBinding' b bs fs

-- | Get the list of all bound inputs @i@ and their corresponding
-- actions for the specified front-end state @s@.
boundActions :: Binding s i -> s -> [(i, Action IO (Binding s i))]
boundActions b state = fmap (\(i, act) -> (i, fmap fst act)) $ boundActions' b () state

-- | Get the list of all bound inputs @i@ and their corresponding
-- actions for the specified back-end state @bs@ and front-end state
-- @fs@.
boundActions' :: Binding' bs fs i -> bs -> fs -> [(i, Action IO (Binding' bs fs i, bs))]
boundActions' b bs fs = M.toList $ unBinding' b bs fs

-- | Get the list of all bound inputs @i@ for the specified front-end
-- state @s@.
boundInputs :: Binding s i -> s -> [i]
boundInputs b s = fmap fst $ boundActions b s

-- | Get the list of all bound inputs @i@ for the specified front-end
-- state @fs@ and the back-end state @bs@.
boundInputs' :: Binding' bs fs i -> bs -> fs -> [i]
boundInputs' b bs fs = fmap fst $ boundActions' b bs fs


-- | A monad to construct 'Binding''. @i@ is the input symbol, and @v@
-- is supposed to be the 'Action' bound to @i@.
newtype Binder i v a = Binder { unBinder :: Writer (Endo [(i, v)]) a }
                       deriving (Monad,Applicative,Functor)

runBinder :: Binder i v a -> [(i, v)] -> [(i, v)]
runBinder = appEndo . execWriter . unBinder

-- | Build a 'Binding' with no explicit or implicit state. The bound
-- actions are activated regardless of the back-end or front-end
-- state.
--
-- If different actions are bound to the same input, the latter action
-- wins.
binds :: Ord i => Binder i (Action IO ()) a -> Binding' bs fs i
binds = binding . flip runBinder []

-- | Build a 'Binding'' with an explicit state (but no implicit
-- state). The bound actions are activated regardless of the back-end
-- or front-end state.
binds' :: Ord i => Binder i (Action (StateT bs IO) ()) a -> Binding' bs fs i
binds' = binding' . flip runBinder []

-- | Create a 'Binder' that binds the action @v@ to the input @i@.
on :: i -> v -> Binder i v ()
on i v = Binder $ tell $ Endo ((i,v) :)

-- | Transform the given action @m a@ into an 'Action' and apply the
-- continuation to it. Usually used as an operator.
run :: (Action m a -> b) -> m a -> b
run cont raw_act = cont $ Action { actDescription = "", actDo = raw_act }

infixl 2 `run`

-- | Transform the given continuation so that the 'ActionDescription'
-- is set to the 'Action' passed to the continuation. Usually used as
-- an operator.
as :: (Action m a -> b) -> ActionDescription -> Action m a -> b
as cont desc act = cont $ act { actDescription = desc }

infixl 2 `as`

-- | Transform the actions in the given 'Binder'.
advice :: (v -> v') -> Binder i v a -> Binder i v' a
advice = undefined


-- | Non-monadic version of 'binds'.
binding :: Ord i => [(i, Action IO ())] -> Binding' bs fs i
binding blist = impl where
  impl = Binding' $ \bs _ -> (fmap . fmap) (const (impl, bs)) $ M.fromList blist

-- | Create a binding that behaves differently for different front-end
-- states @fs@.
ifFront :: (fs -> Bool) -- ^ The predicate
        -> Binding' bs fs i -- ^ Enabled if the predicate is 'True'
        -> Binding' bs fs i -- ^ Enabled if the predicate is 'False'
        -> Binding' bs fs i
ifFront p = ifBoth $ \_ fs -> p fs

-- | Create a binding that behaves differently for different back-end
-- states @bs@.
ifBack :: (bs -> Bool) -- ^ The predicate
       -> Binding' bs fs i -- ^ Enabled if the predicate is 'True'
       -> Binding' bs fs i -- ^ Enabled if the predicate is 'False'
       -> Binding' bs fs i
ifBack p = ifBoth $ \bs _ -> p bs

-- | Create a binding that behaves differently for different front-end
-- and back-end states, @fs@ and @bs@.
ifBoth :: (bs -> fs -> Bool) -- ^ The predicate
       -> Binding' bs fs i -- ^ Enabled if the predicate is 'True'
       -> Binding' bs fs i -- ^ Enabled if the predicate is 'False'
       -> Binding' bs fs i
ifBoth p thenb elseb = Binding' $ \bs fs ->
  if p bs fs
  then mapResult (\nextb -> ifBoth p nextb elseb) id $ unBinding' thenb bs fs
  else mapResult (\nextb -> ifBoth p thenb nextb) id $ unBinding' elseb bs fs

-- | Add a condition on the front-end state to 'Binding'.
whenFront :: (fs -> Bool) -- ^ The predicate.
          -> Binding' bs fs i -- ^ Enabled if the predicate is 'True'
          -> Binding' bs fs i
whenFront p = whenBoth $ \_ fs -> p fs

-- | Add a condition on the back-end state to 'Binding'.
whenBack :: (bs -> Bool) -- ^ The predicate.
         -> Binding' bs fs i -- ^ Enabled if the predicate is 'True'
         -> Binding' bs fs i
whenBack p = whenBoth $ \bs _ -> p bs

-- | Add a condition on the back-end and front-end states to
-- 'Binding'.
whenBoth :: (bs -> fs -> Bool) -- ^ The predicate.
         -> Binding' bs fs i -- ^ Enabled if the predicate is 'True'.
         -> Binding' bs fs i
whenBoth p b = ifBoth p b noBinding

mapResult :: Functor m => (a -> a') -> (b -> b') -> M.Map i (Action m (a, b)) -> M.Map i (Action m (a',b'))
mapResult amapper bmapper = (fmap . fmap) (\(a, b) -> (amapper a, bmapper b))

-- | Contramap the front-end state.
convFront :: (fs -> fs') -> Binding' bs fs' i -> Binding' bs fs i
convFront cmapper orig_bind = Binding' $ \bs fs ->
  mapResult (convFront cmapper) id $ unBinding' orig_bind bs (cmapper fs)

-- | Map the front-end input.
convInput :: Ord i' => (i -> i') -> Binding' bs fs i -> Binding' bs fs i'
convInput mapper orig_bind = Binding' $ \bs fs ->
  mapResult (convInput mapper) id $ M.mapKeys mapper $ unBinding' orig_bind bs fs

-- | Invariant-map the back-end state.
convBack :: (bs -> bs') -> (bs' -> bs) -> Binding' bs fs i -> Binding' bs' fs i
convBack mapper cmapper = extendAt $ Lens.lens cmapper (\_ bs -> mapper bs)

-- | Extend the given 'Binding'' with the given 'Lens.Lens'', so that the
-- 'Binding'' can be part of a 'Binding'' with the bigger state @bs'@
extendAt :: Lens.Lens' bs' bs -- ^ a lens that focuses on @bs@, which is part of the bigger state @bs'@.
         -> Binding' bs fs i
         -> Binding' bs' fs i
extendAt lens orig_bind = Binding' $ \bs' fs ->
  mapResult (extendAt lens) (\bs -> bs' & lens .~ bs) $ unBinding' orig_bind (bs' ^. lens) fs

-- | Convert 'Binding'' to 'Binding' by hiding the explicit state
-- @bs@.
startFrom :: bs -- ^ Initial state
          -> Binding' bs fs i -- ^ Binding' with explicit state
          -> Binding fs i -- ^ Binding containing the state inside
startFrom init_state b' = Binding' $ \() front_state ->
  (fmap . fmap) toB $ unBinding' b' init_state front_state
  where
    toB (next_b', next_state) = (startFrom next_state next_b', ())

-- | Extend 'Binding' to 'Binding''. In the result 'Binding'', the
-- explicit back-end state is just ignored and unmodified.
extend :: Binding fs i -> Binding' bs fs i
extend b = Binding' $ \bs fs ->
  mapResult extend (const bs) $ unBinding' b () fs

-- | Non-monadic version of 'binds''.
binding' :: Ord i => [(i, Action (StateT bs IO) ())] -> Binding' bs fs i
binding' blists = impl where
  impl = Binding' $ \bs _ -> fmap (runStatefulAction impl bs) $ M.fromList $ blists

runStatefulAction :: Binding' bs fs i -> bs -> Action (StateT bs IO) () -> Action IO (Binding' bs fs i, bs)
runStatefulAction next_b' cur_bs state_action =
  state_action { actDo = recursive_io }
  where
  recursive_io = do
    ((), next_bs) <- runStateT (actDo state_action) cur_bs
    return (next_b', next_bs)
