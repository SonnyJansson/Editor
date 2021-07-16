{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module BufString where

import Data.Bifunctor (Bifunctor, bimap)
import Data.FingerTree as FT
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Debug.Trace
import Lens.Micro
import Lens.Micro.Mtl
import Lens.Micro.TH

data Size = Size {
  charIndex :: Int,
  lineIndex :: Int
}

data BufChunk = BufChunk {
  _chunkSize :: {-# UNPACK #-} !Int,
  _fromChunk :: {-# UNPACK #-} !T.Text
} deriving (Eq, Show)

makeLenses ''BufChunk

newtype BufString = BufString {
    fromBufString :: FingerTree Size BufChunk
} deriving(Show)

toChunks :: BufString -> [BufChunk]
toChunks = go . fromBufString
  where
    go (viewl -> c :< cs) = c:(go cs)
    go (viewl -> EmptyL)  = mempty

toTexts :: BufString -> [T.Text]
toTexts = fmap _fromChunk . toChunks

toText :: BufString -> T.Text
toText = mconcat . toTexts

length :: BufString -> Int
length = charIndex . measure . fromBufString

instance Eq BufString where
  s1 == s2 = (BufString.length s1 == BufString.length s2) && (toTexts s1 == toTexts s2)

instance Semigroup Size where
  (Size c1 l1) <> (Size c2 l2) = Size (c1 + c2) (l1 + l2)

instance Monoid Size where
  mempty = Size 0 0

instance Measured Size BufChunk where
  measure c = Size (_chunkSize c) (T.count "\n" $ _fromChunk c)

empty = BufString FT.empty

instance Semigroup BufString where
  (<>) = BufString.append

instance Monoid BufString where
  mempty = BufString.empty

defaultChunkSize = 1200

fromText' :: Int -> T.Text -> BufString
fromText' n = BufString . FT.fromList . fmap toChunk . T.chunksOf n

fromText :: T.Text -> BufString
fromText = fromText' defaultChunkSize

fromString' :: Int -> String -> BufString
fromString' n = fromText' n . T.pack

fromString :: String -> BufString
fromString = fromText . T.pack

toChunk :: T.Text -> BufChunk
toChunk t = BufChunk (T.length t) t

bothMap :: (Bifunctor f) => (a -> b) -> f a a -> f b b
bothMap f = bimap f f

splitAt :: Int -> BufString -> (BufString, BufString)
splitAt n s = bothMap BufString $ case (viewl rt) of
                (BufChunk len tx :< ts) | nTx /= 0 ->
                    let (ltx, rtx) = T.splitAt nTx tx
                    in (lt |> BufChunk nTx ltx,
                        BufChunk (len - nTx) rtx <| ts)
                _ -> (lt, rt)
  where
    (lt, rt) = split ((> n) . charIndex) $ fromBufString s
    nTx = n - charIndex (measure lt)

splitAtLine :: Int -> BufString -> (BufString, BufString)
splitAtLine n s = bothMap BufString $ case (viewl rt) of
                (BufChunk len tx :< ts) | nTx /= 0 ->
                    let (ltx, rtx) = T.splitAt nTx tx
                    in (lt |> BufChunk nTx ltx,
                        BufChunk (len - nTx) rtx <| rt)
                _ -> (lt, rt)
  where
    (lt, rt) = split ((> n) . lineIndex) $ fromBufString s
    nTx = n - lineIndex (measure lt)

append :: BufString -> BufString -> BufString
append (BufString t1) (BufString t2) = case (viewr t1, viewl t2) of
  (EmptyR, _) -> BufString t2
  (_, EmptyL) -> BufString t1
  (ts1 :> BufChunk l1 tx1, BufChunk l2 tx2 :< ts2) ->
    let len = l1 + l2 in case compare len defaultChunkSize of
      GT -> BufString $ t1 <> t2
      _  -> BufString $ (ts1 |> (BufChunk len (tx1 <> tx2))) <> ts2

head :: BufString -> Maybe Char
head (BufString t) = case viewl t of
  EmptyL               -> Nothing
  BufChunk _ tx :< _ -> if T.null tx then Nothing else Just $ T.head tx

last :: BufString -> Maybe Char
last (BufString t) = case viewr t of
  EmptyR               -> Nothing
  _ :> BufChunk _ tx -> if T.null tx then Nothing else Just $ T.last tx

reChunk :: BufString -> BufString
reChunk = fromText . toText

readFile :: FilePath -> IO BufString
readFile = fmap fromText . TIO.readFile 

