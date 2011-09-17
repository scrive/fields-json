module Text.JSON.Fields where

import Text.JSON
import Text.JSON.ToJSON
import Data.Map
import Data.Map as Map
import Control.Monad.State.Strict

type Fields = State ([(String, IO JSValue)]) ()


class Field a where
  field :: String -> a -> Fields
  
instance (ToJSON a) => Field a where  
  field n v = modify $ \s -> (n, return $ toJSON v) : s

instance (ToJSON a) => Field (IO a) where  
  field n v = modify $ \s -> (n, fmap toJSON v) : s

instance (ToJSON a) => Field (Fields) where  
  field n v = modify $ \s -> (n, val) : s
   where
      val = toJSON . fmap Map.fromList (sequence $ fmap packIO $ execState v [])
  
instance (ToJSON a) => Field [Fields m] where  
  field n fs = modify $ \s -> (n, fmap toJSON $ mapM vals fs) : s
      where
        vals f = fmap Map.fromList  (sequence $ fmap packIO $ execState f [])

packIO :: (a, m b) -> m (a, b)
packIO (name, comp)= do
    res <- comp
    return (name,res)