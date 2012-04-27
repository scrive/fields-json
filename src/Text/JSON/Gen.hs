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

 module Text.JSON.Gen (
    module Text.JSON.ToJSValue
  , JSONGen
  , runJSONGen
  , JSONGenT
  , runJSONGenT
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
import Data.Foldable
import Data.Sequence as S
import Text.JSON
import Text.JSON.ToJSValue

type JSONGen = JSONGenT Identity

runJSONGen :: JSONGen () -> JSValue
runJSONGen = runIdentity . runJSONGenT

newtype JSONGenT m a = JSONGenT (StateT (Seq (String, JSValue)) m a)
  deriving (Applicative, Functor, Monad, MonadTrans)

instance MonadIO m => MonadIO (JSONGenT m) where
  liftIO = JSONGenT . liftIO

runJSONGenT :: Monad m => JSONGenT m () -> m JSValue
runJSONGenT (JSONGenT f) = (JSObject . toJSObject . toList) `liftM` execStateT f S.empty

value :: (Monad m, ToJSValue a) => String -> a -> JSONGenT m ()
value name val = JSONGenT $ modify (|> (name, toJSValue val))

valueM :: (Monad m, ToJSValue a) => String -> m a -> JSONGenT m ()
valueM name mval = lift mval >>= value name

object :: Monad m => String -> JSONGenT m () -> JSONGenT m ()
object name json = JSONGenT $ do
  val <- lift $ runJSONGenT json
  modify (|> (name, toJSValue val))

objects :: Monad m => String -> [JSONGenT m ()] -> JSONGenT m ()
objects name jsons = JSONGenT $ do
  val <- mapM (lift . runJSONGenT) jsons
  modify (|> (name, toJSValue val))

