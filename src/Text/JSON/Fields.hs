{-# LANGUAGE OverlappingInstances, IncoherentInstances #-}
module Text.JSON.Fields where

import Text.JSON
import Text.JSON.ToJSON
import Data.Map as Map
import Control.Monad.State.Strict


type Fields = State ([(String, IO JSValue)]) ()

json :: Fields -> IO JSValue
json fields = fmap (JSObject . toJSObject) $ sequence $ fmap pack $ execState fields []

class Field a where
  field :: String -> a -> Fields
  
instance (ToJSON a) => Field a where  
  field n v = modify $ \s -> (n, return $ toJSON v) : s

instance (ToJSON a) => Field (IO a) where  
  field n v = modify $ \s -> (n, fmap toJSON v) : s

instance Field (Fields) where  
  field n v = modify $ \s -> (n, fmap toJSON val) : s
   where
      val = fmap Map.fromList (sequence $ fmap pack $ execState v [])
  
instance Field [Fields] where  
  field n fs = modify $ \s -> (n, fmap toJSON $ mapM vals fs) : s
      where
        vals f = fmap Map.fromList  (sequence $ fmap pack $ execState f [])

pack :: (Functor f) => (a, f b) -> f (a, b)
pack (name, comp)=  fmap (\res -> (name,res)) comp
