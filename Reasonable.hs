{-# LANGUAGE DeriveFunctor #-}
-- | 

module Reasonable where

import Control.Monad.Free

data Interact a = Ask String
                | Tell String
                deriving (Show, Eq, Functor)

tell :: String -> Free Interact ()
tell msg = liftF $ Tell msg

ask :: String -> Free Interact String
ask prompt = liftF $ Ask prompt

prg :: Free Interact ()
prg = do
  first <- ask "What is your first name?"
  last  <- ask "What is your last name?"
  tell $ "Hello, " ++ first ++ " " ++ last


showProgram :: Free Interact a -> String
showProgram (Free (Ask prompt)) = "ask " ++ prompt
showProgram (Free (Tell msg)) = "tell " ++ msg
