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
-- Unifing some structures to JSValue
--


module Text.JSON.ToJSON (ToJSON(..))where

import Text.JSON
import Data.Map

class ToJSON a where
  toJSON:: a -> JSValue

instance ToJSON JSValue where
  toJSON = id
  
instance ToJSON String where
  toJSON = JSString . toJSString 

instance ToJSON Bool where
  toJSON = JSBool

instance ToJSON Int where
  toJSON = JSRational True . toRational
  
instance ToJSON Integer where
  toJSON = JSRational True . toRational
  
instance (ToJSON a) =>  ToJSON [a] where
  toJSON = JSArray . fmap toJSON 
  
instance (ToJSON a) => ToJSON (Map String a) where  
  toJSON = JSObject . toJSObject . toList . (fmap toJSON) 