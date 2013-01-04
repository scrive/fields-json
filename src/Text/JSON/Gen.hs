-----------------------------------------------------------------------------
-- |
-- Module      :  Text.JSON.Gen
-- Copyright   :  (c) Scrive 2011
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  andrzej@scrive.com
-- Stability   :  development
-- Portability :  portable
--
-- Abusing monadic 'do' notation library for generating JSON object. 
-- Hard-binded to json package from hackage.
-- Main ideas
--
-- * Overloaded function 'value' to set values in underlying JSON - 'Bool', 'Int', 'String', lists  etc.
--
-- * JSON generation may not be pure  with 'valueM'. You can perform some IO while generating JSON. This is usefull skip useless inner binding.
--
-- * Compositionality - use 'object' to easy create JSON objects
--
-- * Monadic notation - it really looks nicer then composition with '.' or some magic combinator
--
-- >
-- > runJSONGen $ do
-- >     value "a" "a"
-- >     value "b" [1,2,3]
-- >     object "c" $ do
-- >         value "x" True
-- >         value "y" False
-- >
-- 
-- Will generate json object 
--  {a : "a", b: [1,2,3], c: {x: true, y : false}} 


 module Text.JSON.Gen (
    module Text.JSON.ToJSValue
    -- * Basic types
  , JSONGen
  , JSONGenT
    -- * Runners
  , runJSONGen
  , runJSONGenT
    -- * Creating JSON's
  , value
  , valueM
  , object
  , objects
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad.State.Strict
import Control.Monad.Reader

import Data.Sequence as S
import Text.JSON
import Text.JSON.ToJSValue
import Text.JSON.JSValueContainer

-- --------------------------------------------------------------

-- | Basic types
type JSONGen = JSONGenT Identity

newtype JSONGenT m a = JSONGenT (StateT (Seq (String, JSValue)) m a)
  deriving (Applicative, Functor, Monad, MonadTrans)


-- | This instance gives us the ability to use FromJSValue function while generating.
instance (Monad m) => MonadReader (Seq (String, JSValue)) (JSONGenT m) where
    ask = JSONGenT (get)
    local f (JSONGenT m) = JSONGenT $ do
                             s <- get
                             put (f s)
                             res <- m
                             put s
                             return res


instance MonadIO m => MonadIO (JSONGenT m) where
  liftIO = JSONGenT . liftIO

-- --------------------------------------------------------------

-- | Simple runner
runJSONGen :: JSONGen () -> JSValue
runJSONGen = runIdentity . runJSONGenT


runJSONGenT :: Monad m => JSONGenT m () -> m JSValue
runJSONGenT (JSONGenT f) = getJSValue `liftM` execStateT f S.empty

-- --------------------------------------------------------------

-- | Set pure value under given name in final JSON object
value :: (Monad m, ToJSValue a) => String -> a -> JSONGenT m ()
value name val = JSONGenT $ modify (|> (name, toJSValue val))

-- | Monadic verion of 'value'
valueM :: (Monad m, ToJSValue a) => String -> m a -> JSONGenT m ()
valueM name mval = lift mval >>= value name


-- | Embed other JSON object as field in resulting JSON object.
object :: Monad m => String -> JSONGenT m () -> JSONGenT m ()
object name json = JSONGenT $ do
  val <- lift $ runJSONGenT json
  modify (|> (name, toJSValue val))


-- | Version for lists of objects.  
objects :: Monad m => String -> [JSONGenT m ()] -> JSONGenT m ()
objects name jsons = JSONGenT $ do
  val <- mapM (lift . runJSONGenT) jsons
  modify (|> (name, toJSValue val))

