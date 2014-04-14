-- {-# language RankNTypes #-}

module Control.Monad.Distribute where

import Control.Monad.Morph

import qualified Control.Monad.Trans.State as S
import qualified Control.Monad.Trans.State.Strict as SS

import qualified Control.Monad.Trans.Reader as R

import qualified Control.Monad.Trans.Error as E

import qualified Control.Monad.Trans.Identity as I

import qualified Control.Monad.Trans.Maybe as M

import qualified Control.Monad.Trans.Writer as W
import qualified Control.Monad.Trans.Writer.Strict as WS

import qualified Control.Monad.Trans.List as LT

import qualified Control.Monad.Trans.RWS as RWS

import qualified Control.Monad.Trans.RWS.Strict as RWSS


import qualified Control.Monad.Trans.Cont as Cont

import Data.Monoid

import Control.Applicative.Backwards (Backwards (Backwards))

class MDistribute t where
    distribute :: (MonadTrans t1, MFunctor t1, Monad (t1 (t m)), Monad m) => t (t1 m) r -> t1 (t m) r


instance MDistribute (S.StateT s) where
    distribute = distributeState

instance MDistribute (SS.StateT s) where
    distribute = distributeStateStrict


instance MDistribute (R.ReaderT i) where
    distribute = distributeReader


instance (Monoid w) => MDistribute (W.WriterT w) where
    distribute = distributeWriter

instance (Monoid w) => MDistribute (WS.WriterT w) where
    distribute = distributeWriterStrict


instance (E.Error e) => MDistribute (E.ErrorT e) where
    distribute = distributeError


instance MDistribute (I.IdentityT) where
    distribute = distributeIdentity

instance (Monoid w) => MDistribute (RWS.RWST i w s) where
    distribute = distributeRWS

instance (Monoid w) => MDistribute (RWSS.RWST i w s) where
    distribute = distributeRWSStrict


instance MDistribute (LT.ListT) where
    distribute = distributeList


distributeState m = stateG (\s -> flip S.runStateT s m)
{-# INLINABLE distributeState #-}

distributeStateStrict m = stateGS (\s -> flip SS.runStateT s m)
{-# INLINABLE distributeStateStrict #-}

distributeReader m = readerG (\i -> flip R.runReaderT i m)
{-# INLINABLE distributeReader #-}

distributeWriter = writerG . W.runWriterT
{-# INLINABLE distributeWriter #-}

distributeWriterStrict = writerGS . WS.runWriterT
{-# INLINABLE distributeWriterStrict #-}

distributeMaybe = maybeG . M.runMaybeT
{-# INLINABLE distributeMaybe #-}

distributeList = listG . LT.runListT
{-# INLINABLE distributeList #-}

distributeError = errorG . E.runErrorT
{-# INLINABLE distributeError #-}

distributeIdentity = identityG . I.runIdentityT
{-# INLINABLE distributeIdentity #-}

distributeRWS m = rwsG (\w s -> RWS.runRWST m w s)
{-# INLINABLE distributeRWS #-}

distributeRWSStrict m = rwsGS (\w s -> RWSS.runRWST m w s)
{-# INLINABLE distributeRWSStrict #-}

-- distributeCont m = contG (\fm -> Cont.ContT fm)
-- {-# INLINABLE distributeRWSStrict #-}

stateG k = do
    s <- lift S.get
    (r, s') <- hoist lift (k s)
    lift (S.put s')
    return r
{-# INLINABLE stateG #-}


{- | Strict version
 -}
stateGS k = do
    s <- lift SS.get
    (r, s') <- hoist lift (k s)
    lift (SS.put s')
    return r
{-# INLINABLE stateGS #-}

readerG k = do
    i <- lift R.ask
    hoist lift (k i)
{-# INLINABLE readerG #-}

writerG p = do
    (r, w) <- hoist lift p
    lift $ W.tell w
    return r
{-# INLINABLE writerG #-}

writerGS p = do
    (r, w) <- hoist lift p
    lift $ WS.tell w
    return r
{-# INLINABLE writerGS #-}

maybeG p = do
    x <- hoist lift p
    lift $ M.MaybeT (return x)
{-# INLINABLE maybeG #-}

listG p = do
    x <- hoist lift p
    lift $ LT.ListT (return x)
{-# INLINABLE listG #-}

listG' p = do
    x <- hoist lift p
    lift $ LT.ListT (return x)

errorG m = do
    x <- hoist lift m
    lift $ E.ErrorT (return x)
{-# INLINABLE errorG #-}

identityG m = do
    x <- hoist lift m
    lift $ I.IdentityT (return x)
{-# INLINABLE identityG #-}

rwsG k = do
    i <- lift RWS.ask
    s <- lift RWS.get
    (r, s', w) <- hoist lift (k i s)
    lift $ do
        RWS.put s'
        RWS.tell w
    return r
{-# INLINABLE rwsG #-}

rwsGS k = do
    i <- lift RWSS.ask
    s <- lift RWSS.get
    (r, s', w) <- hoist lift (k i s)
    lift $ do
        RWSS.put s'
        RWSS.tell w
    return r
{-# INLINABLE rwsGS #-}

{-
backwardsG m = do
    x <- hoist lift m
    lift $ Backwards (return x)
-- {-# INLINABLE identityG #-}
-}

{-
contG m = do
    x <- hoist lift m
    lift $ Cont.ContT (return x)

contGdr m = do
    x <- hoist lift m
    lift $ Cont.ContT (return . return $ x)

contG' m = do
    x <- hoist lift (Cont.runContT m Cont.callCC)
    lift $ Cont.ContT (return x)

contG'' m = do
    x <- hoist lift (Cont.runContT m return)
    lift $ Cont.ContT (return x)


contG''' m = do
    x <- hoist lift (m  ())
    lift $ Cont.ContT (return x)

contG'''' m = do
    x <- hoist lift (m  undefined)
    lift $ Cont.ContT (return . return $ x)

{- only problem what should undefined be?
contG'''' . Cont.runContT
  :: (Monad m, Monad (t (Cont.ContT a1 m)), MonadTrans t,
      MFunctor t) =>
     Cont.ContT a1 (t m) a -> t (Cont.ContT a1 m) b
-}

-}


test = do
    a <- LT.ListT . return $ [1..2]
    b <- LT.ListT . return $ [1..2]
    lift $ S.modify (+1)
    s <- lift $ S.get
    return (a, b, s)
