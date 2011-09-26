-----------------------------------------------------------------------------
-- |
-- Module      :  Text.JSON.Fields
-- Copyright   :  (c) Scrive 2011
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  mariusz@scrive.com
-- Stability   :  development
-- Portability :  portable
--
-- Abusing monadic do notation library for generating JSON object. 
-- Hard-binded to json package from hackage.
-- Main ideas
--
-- * Overloaded function 'field', that may set values of fields of different types - 'Bool', 'Int', 'String', lists  etc.
--
-- * Internal IO - value of the field can be IO a, is we know how to put a into JSON. That means that there is no need to do prebinding 
--
-- * Compositionality - value of field can also be fields. Easy to do embeded  objects
--
-- * Monadic notation - it really looks nicer then composition with '.' or some magic combinator
--
-- >
-- > json $ do
-- >     field "a" "a"
-- >     field "b" [1,2,3]
-- >     field "c" $ do
-- >         field "x" True
-- >         field "y" False
-- >
-- 
-- Will generate json object 
--  {a : "a", b: [1,2,3], c: {x: true, y : false}} 
--

module Text.JSON.Fields (json, JSField(..))where

import Text.JSON
import Text.JSON.ToJSON
import Data.Map as Map
import Control.Monad.State.Strict


type JSFields = State ([(String, IO JSValue)]) ()


{- | Function for changing 'JSFields' into real JSON object -}

json :: JSFields -> IO JSValue
json fields = fmap (JSObject . toJSObject) $ sequence $ fmap pack $ execState fields []

{- | The 'JSField' class instances are object that in some sence can be interpreted as value of JSON object fields. 
     To derive new instances use existing instances since internal structure 'JSFields' is hidden.
-}

class JSField a where
  field :: String -> a -> JSFields
  
instance (ToJSON a) => JSField a where  
  field n v = modify $ \s -> (n, return $ toJSON v) : s

instance (ToJSON a) => JSField (IO a) where  
  field n v = modify $ \s -> (n, fmap toJSON v) : s

instance JSField (JSFields) where  
  field n v = modify $ \s -> (n, fmap toJSON val) : s
   where
      val = fmap Map.fromList (sequence $ fmap pack $ execState v [])
  
instance JSField [JSFields] where  
  field n fs = modify $ \s -> (n, fmap toJSON $ mapM vals fs) : s
      where
        vals f = fmap Map.fromList  (sequence $ fmap pack $ execState f [])

pack :: (Functor f) => (a, f b) -> f (a, b)
pack (name, comp)=  fmap (\res -> (name,res)) comp
