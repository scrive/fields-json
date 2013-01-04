-----------------------------------------------------------------------------
-- |
-- Module      :  Text.JSON.ToJSON
-- Copyright   :  (c) Scrive 2011
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  mariusz@scrive.com
-- Stability   :  development
-- Portability :  portable
--
-- Data structures that hold JSValue inside.
-- Value can be extracted or replaced, but is always inside.
--


module Text.JSON.JSValueContainer (JSValueContainer(..))where

import Text.JSON
import Data.Sequence as S
import Data.Foldable

class JSValueContainer a where
    getJSValue :: a -> JSValue
    setJSValue :: JSValue -> a -> a

instance JSValueContainer JSValue where
    getJSValue = id
    setJSValue= const

-- Instance used by generator. Note that putting is not ~ workeround.
instance JSValueContainer (Seq (String, JSValue)) where
    getJSValue s = JSObject $ toJSObject $ toList s
    setJSValue (JSObject o) _ = S.fromList $ fromJSObject o
    setJSValue _ s = s -- We could throw error, but why should we bothered