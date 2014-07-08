{-# LANGUAGE DeriveFunctor #-}
-- | 

module Reasonable where

import Control.Monad.Free
  
data Interact next = Ask String (String -> next)
                   | Tell String next
                   deriving Functor

tell :: String -> Free Interact ()
tell msg = liftF $ Tell msg ()

ask :: String -> Free Interact String
ask prompt = liftF $ Ask prompt id

prg :: Free Interact ()
prg = do
  first <- ask "What is your first name?"
  last  <- ask "What is your last name?"
  tell $ "Hello, " ++ first ++ " " ++ last

showProgram :: (Show a) => Free Interact a -> String
showProgram (Free (Ask prompt f)) = "ask " ++ prompt ++ "\n" ++ (showProgram (f "\"hello\""))
showProgram (Free (Tell msg next)) = "tell " ++ msg ++ "\n" ++ (showProgram next)
showProgram (Pure a) = show a

runInteract :: Free Interact a -> IO a
runInteract (Free (Ask prompt fnext)) = do
  putStr $ prompt ++ " "
  answer <- getLine
  runInteract $ fnext answer
runInteract (Free (Tell msg next)) = do
  putStrLn msg
  runInteract next
runInteract (Pure a) = return a

interactIO :: Interact a -> IO a
interactIO (Ask prompt fnext) = do
  putStr $ prompt ++ " "
  answer <- getLine
  return $ fnext answer
interactIO (Tell msg next) = do
  putStrLn msg
  return $ next

runInteract' :: Free Interact a -> IO a
runInteract' = retract . hoistFree interactIO
