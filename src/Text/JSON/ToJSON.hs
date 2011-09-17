module Text.JSON.ToJSON where

import Text.JSON
import Data.Map

class ToJSON a where
  toJSON:: a -> JSValue
  
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