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
-- Unifing some structures so they can be serialized to JSValue
--


module Text.JSON.ToJSValue (ToJSValue(..))where

import Text.JSON
import Data.Map as M

class ToJSValue a where
  toJSValue :: a -> JSValue

instance ToJSValue JSValue where
  toJSValue = id

instance ToJSValue Bool where
  toJSValue = JSBool

instance ToJSValue String where
  toJSValue = JSString . toJSString

instance Real a => ToJSValue a where
  toJSValue = JSRational True . toRational

instance ToJSValue a => ToJSValue [a] where
  toJSValue = JSArray . fmap toJSValue

instance ToJSValue a => ToJSValue (M.Map String a) where
  toJSValue = JSObject . toJSObject . M.toList . fmap toJSValue

instance ToJSValue a => ToJSValue (Maybe a) where
  toJSValue = maybe JSNull toJSValue

instance (ToJSValue a, ToJSValue b) => ToJSValue (a,b) where
  toJSValue (a,b) = JSArray [toJSValue a, toJSValue b]
