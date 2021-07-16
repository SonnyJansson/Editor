{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Editor where

import Control.Arrow (first, (&&&))
import Control.Monad.State
import Data.FingerTree as FT
import Data.Functor.Identity
-- import Data.List.NonEmpty as NE hiding ((<|))
import Data.Hashable
import qualified Data.Map as M
import Data.Tuple (swap)
import qualified Data.Text as T
import Lens.Micro
import Lens.Micro.Mtl
import Lens.Micro.TH

import Unsafe.Coerce

import BufString

-- BUFFER MONAD --

data Cursor = Cursor {
  _cursorLoc :: Int,
  _cursorRow :: Int,
  _cursorPrefCol :: Int
  } deriving (Eq, Show)

makeLenses ''Cursor

data Buffer = Buffer {
  _bufString :: BufString,
  _cursor :: Cursor
  } deriving (Eq, Show)

makeLenses ''Buffer

 -- data BufM s a = BufM {
 --   runBufState :: s -> (a, s),
 --   unRunBufState :: s -> s -> s
 -- }

newtype RevStateT s m a = RevStateT {
  runRevStateT :: s -> m ((a, s), s -> s)
}

type RevState s = RevStateT s Identity

type BufM = RevState Buffer

runRevState :: RevState s a -> s -> ((a, s), s -> s)
runRevState m = runIdentity . runRevStateT m

runBuf :: BufM a -> Buffer -> ((a, Buffer), Buffer -> Buffer)
runBuf = runRevState

instance Functor m => Functor (RevStateT s m) where
  fmap f b = RevStateT $ \s -> fmap ((first . first) f) $ runRevStateT b s

instance Monad m => Applicative (RevStateT s m) where
  pure = return
  (<*>) = ap

instance Monad m => Monad (RevStateT s m) where
 return a = RevStateT $ \s -> return ((a, s), id)

 b >>= f = RevStateT $ \s -> do
   ((ba, bs), bInv) <- runRevStateT b s
   ((fa, fs), fInv) <- runRevStateT (f ba) bs
   return ((fa, fs), (bInv. fInv))


instance Monad m => MonadState s (RevStateT s m) where
  get = RevStateT $ \s -> return ((s, s), id)
  put s = RevStateT $ const $ return (((), s), id)

withoutResult :: s -> ((), s)
withoutResult s = ((), s)

-- simpleInv :: BufM a -> BufM ()
-- simpleInv b = buf $ \s -> 
--   let ((_, s'), f) = runBuf b s

buf :: (Buffer -> ((a, Buffer), Buffer -> Buffer)) -> BufM a
buf f = RevStateT $ Identity . f

makeBuf :: (Buffer -> (a, Buffer)) -> (Buffer -> Buffer -> Buffer) -> BufM a
makeBuf f r = buf $ f &&& r

makeBuf' :: (Buffer -> Buffer) -> (Buffer -> Buffer -> Buffer) -> BufM ()
makeBuf' f = makeBuf (withoutResult . f)

simpleBuf :: (Buffer -> (a, Buffer)) -> (Buffer -> Buffer) -> BufM a
simpleBuf f r = makeBuf f (const r)

simpleBuf' :: (Buffer -> Buffer) -> (Buffer -> Buffer) -> BufM ()
simpleBuf' f = simpleBuf (withoutResult . f)

-- Uses a lens to fetch a value out of a structure of type s and put it into another structure of the same type
-- Useful for easily making reverse functions, by simply having to specify a lens pointing to a value
restore :: Lens' s a -> s -> s -> s
restore l b = l .~ (b ^. l)

-- {-# RULES
-- "restore/restore" forall s (s1 :: s) (s2 :: s) (l :: forall a. Lens s a). restore l s2 . (restore l s1) = restore l s2
-- #-}

nextLine :: BufM ()
nextLine = makeBuf' action reverse
  where
    action = cursor . cursorRow +~ 1
    reverse = restore (cursor . cursorRow)

-- prevLine :: BufM ()
-- prevLine = simpleBuf' (cursor . cursorRow -~ 1) (cursor . cursorRow +~ 1) 

prevLine :: BufM ()
prevLine = makeBuf' action reverse
  where
    action = cursor . cursorRow -~ 1
    reverse = restore (cursor . cursorRow)

-- insert :: T.Text -> BufM ()
-- insert t = simpleBuf' (insert text into buffer here) (toRev $ delete $ length t)

insertAt' :: BufString -> Int -> BufString -> BufString
insertAt' s n str = left `append` s `append` right
  where
    (left, right) = BufString.splitAt n str

deleteBetween' :: Int -> Int -> BufString -> BufString
deleteBetween' from to str = left `append` right
  where
    (left, right') = BufString.splitAt from str
    (_, right) = BufString.splitAt (to - from) right'

substr' :: Int -> Int -> BufString -> BufString
substr' from to str = mid
  where
    (_, right') = BufString.splitAt from str
    (mid, _) = BufString.splitAt (to - from) right'

insertAt :: BufString -> Int -> BufM ()
insertAt s n = simpleBuf' action reverse
  where
    action = bufString %~ insertAt' s n
    reverse = bufString %~ deleteBetween' n (n + BufString.length s)

deleteBetween :: Int -> Int -> BufM ()
deleteBetween from to = makeBuf' action reverse
  where
    action = bufString %~ deleteBetween' from to
    reverse s = let sub = substr' from to (s ^. bufString)
                in bufString %~ insertAt' sub from

data UndoList = (BufM (), Buffer -> Buffer) :| UndoList
              | Nil (BufM ())

type RedoList = [BufM ()]

data ActionList = ActionList {
  redoList :: RedoList,
  undoList :: UndoList
}

emptyActionList :: ActionList
emptyActionList = ActionList [] (Nil $ return ())

addSeparateAction :: BufM () -> (Buffer -> Buffer) -> ActionList -> ActionList
addSeparateAction act rev (ActionList _ undo) = ActionList [] ((act, rev) :| undo)

addCombinedAction :: BufM () -> (Buffer -> Buffer) -> ActionList -> ActionList
addCombinedAction act rev (undoList -> (act', rev') :| undo) =
  ActionList [] ((act' >> act, rev' . rev) :| undo)
addCombinedAction act rev (undoList -> Nil act') =
  ActionList [] (Nil (act >> act'))

popUndoAction :: ActionList -> Maybe (BufM (), (Buffer -> Buffer), ActionList)
popUndoAction (ActionList redo ((act, rev) :| undo)) = Just (act, rev, ActionList redo undo)
popUndoAction (ActionList redo undo) = Nothing

pushUndoAction :: (BufM (), Buffer -> Buffer) -> ActionList -> ActionList
pushUndoAction act (ActionList redo undo) = ActionList redo (act :| undo)

popRedoAction :: ActionList -> Maybe (BufM (), ActionList)
popRedoAction (ActionList (a:as) undo) = Just (a, ActionList as undo)
popRedoAction (ActionList [] undo) = Nothing

pushRedoAction :: BufM () -> ActionList -> ActionList
pushRedoAction act (ActionList redo undo) = ActionList (act:redo) undo

-- For generalised undo-redo data structures (some people do not like undo trees)
class Actions a where
  undo :: Buffer -> a -> (Buffer, a)
  redo :: Buffer -> a -> (Buffer, a)

type BufId = Int

type ListenerID = Int

data Listener where
  Listener :: (a -> EditorM ()) -> Listener

type HookID = Int

data HookManager = HookManager {
  _nextId :: ListenerID,
  _listeners :: M.Map HookID [(ListenerID, Listener)]
}

data Editor = Editor {
  _actionList :: ActionList,
  _buffers :: M.Map BufId Buffer,
  _hookManager :: HookManager
}

type EditorM = State Editor

makeLenses ''Editor

newtype Hook a = Hook { hookId :: HookID }

makeLenses ''HookManager

runEditor :: EditorM a -> Editor -> (a, Editor)
runEditor = runState

performAction :: Bool -> BufId -> BufM () -> EditorM ()
performAction sep bufId action = do
  bufs <- use buffers
  let ((_, buf), rev) = runBuf action (bufs ! bufId)
  buffers %= M.insert bufId buf
  -- actionList %= Left . cons action
  actionList %= (if sep then addSeparateAction else addCombinedAction) action rev 
  return ()
    
undoAction :: BufId -> EditorM ()
undoAction bufId = do
  actList <- use actionList
  case popUndoAction actList of
    Just (act, rev, actList') -> do
      buffers %= M.adjust rev bufId
      actionList .= pushRedoAction act actList'
    Nothing -> return ()
  return ()

redoAction :: BufId -> EditorM ()
redoAction bufId = do
  actList <- use actionList
  case popRedoAction actList of
    Just (act, actList') -> do
      bufs <- use buffers
      let ((_, buf), rev) = runBuf act (bufs ! bufId)
      buffers %= M.insert bufId buf
      actionList .= pushUndoAction (act, rev) actList'
      -- performAction True bufId act -- This should not run performAction! That removes redos.
    Nothing -> return ()
  return ()

newHook :: String -> Hook a
newHook = Hook . hash

listenersOnHook :: Hook a -> EditorM [a -> EditorM ()]
listenersOnHook (Hook hookId) = do
  lst <- use (hookManager . listeners)
  return $ fmap (\(Listener id f) -> unsafeCoerce f) (lst ! hookId)

dispatchOnHook :: Hook a -> a -> EditorM ()
dispatchOnHook hook message =
  listenersOnHook hook >>= mapM_ ($ message)

addListener :: Hook a -> (a -> EditorM ()) -> EditorM ListenerID
addListener (Hook hookId) listener = do
  let id = use (hookManager . nextId)
  hookManager . nextId += 1
  hookManager . listeners %= M.adjust ((id, Listener listener):) hookId

removeListener :: Hook a -> ListenerID -> EditorM ()
removeListener (Hook hookId) listenerId = do
  hookManager . listeners %= M.adjust (removeMatchingId listenerId)
    where
      removeMatchingId :: ListenerID -> [Listener] -> [Listener]
      removeMatchingId _ [] = []
      removeMatchingId id (lst@(id', _):xs) =
        if id == id' then xs else lst:(removeMatchingId xs)

data ModeStatus = Enabled | Disabled

type IsEnabled :: ModeStatus -> Bool
type family IsEnabled ms where
  IsEnabled Enabled  = True
  IsEnabled Disabled = False

type IfThenElse :: Bool -> * -> * -> *
type family IfThenElse ms t1 t2 where
  IfThenElse True  a _ = a
  IfThenElse False b _ = b

data Mode s (ms :: ModeStatus) = Mode {
  -- hookList :: Either [(Hook a, a -> EditorM ())] [(Hook a, ListenerID)]
  modeName :: String,
  modeHookList :: [(HookID, IfThenElse (IsEnabled ms) ListenerID Listener)]
}

setupHooks :: [forall a. (Hook a, a -> EditorM ())] -> State (Mode s Disabled) ()
setupHooks listeners = do
  listenerIds <- mapM (\(hook, listener) -> addListener hook listener) listeners
  let removeList = zip (map fst listeners) listenerIds
  return ()
  

-- emptyActionList :: ActionList
-- emptyActionList = return $ return ()
-- 
-- addSeparateAction :: BufM () -> ActionList -> ActionList
-- addSeparateAction = cons
-- 
-- addCombinedAction :: BufM () -> ActionList -> ActionList
-- addCombinedAction a (x:|xs) = (x >> a):|xs
