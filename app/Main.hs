module Main where

import Data.Map as M
import Data.Maybe (fromJust)

import BufString
import Editor

main :: IO ()
main = do
  putStrLn $ show $ _bufString buffer
  putStrLn $ show $ _bufString $ fromJust $ M.lookup 1 $ _buffers $ snd $ runEditor editorTest editor

editorTest :: EditorM ()
editorTest = do
  performAction True 1 $ insertAt (fromString "Weee") 5
  performAction False 1 $ insertAt (fromString "Wooo") 17
  undoAction 1
  redoAction 1

textTests :: IO ()
textTests = do
  putStrLn "Hello, Haskell!"
  putStrLn $ show text
  let wee = fromString "Weeeeeeee!"
  let text' = insertAt' wee 10 text
  putStrLn $ show text'
  putStrLn "Rechunking!"
  putStrLn $ show $ reChunk text'
  putStrLn $ show $ deleteBetween' 10 (10 + BufString.length wee) text'

-- text = (fromString $ concat $ replicate 50 "Hello, this is just a little test!") <> (fromString "Pretty neat, eh?")
text = fromString "Hello, this is a short test text!"

buffer :: Buffer
buffer = Buffer {
  _bufString = text,
  _cursor = Cursor 0 0 0
}

editor :: Editor
editor = Editor {
  _actionList = emptyActionList,
  _buffers = M.singleton 1 buffer,
  _listeners = M.empty
}
