-- |
-- Module: WildBind.X11.Internal.Queue
-- Description: A simple queue implementation.
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- This is an internal module. End-users should not reply on this.
module WildBind.X11.Internal.Queue (
  Queue,
  empty, unshift, unshiftAll, pop
) where

newtype Queue a = Queue [a] deriving (Show, Eq)

empty :: Queue a
empty = Queue []

unshift :: Queue a -> a -> Queue a
unshift (Queue alist) aelem = Queue (aelem : alist)

unshiftAll :: Queue a -> [a] -> Queue a
unshiftAll q [] = q
unshiftAll q (aelem:rest) = unshiftAll (unshift q aelem) rest

pop :: Queue a -> (Maybe a, Queue a)
pop q@(Queue []) = (Nothing, q)
pop (Queue [aelem]) = (Just aelem, Queue [])
pop (Queue (head_elem : rest)) = let (ret_elem, Queue ret_alist) = pop (Queue rest)
                                 in (ret_elem, Queue (head_elem : ret_alist))
