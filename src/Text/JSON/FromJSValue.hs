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
-- Interface for extracting data from JSValue.
--


module Text.JSON.FromJSValue (
                               -- * Basic Parsing
                                 FromJSValue(..)
                               -- * Data Extraction
                               , fromJSValueField
                               , fromJSValueFieldBase64
                               , fromJSValueFieldCustom
                               , fromJSValueCustomMany
                               , fromJSValueCustomList
                               -- * Running
                               , withJSValue  ) where
import Text.JSON
import Text.JSON.JSValueContainer
import Control.Monad
import Control.Monad.Reader
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString.Base64 as BASE64
import Data.Ratio
import Control.Monad.Identity
import Data.Maybe
import Text.JSON.String


-- | Structures that can be 'parsed' from JSON. Instances must declare either 'fromJSValue' (parse directly from 'JSValue') or 'fromJSValueM' (uses 'MonadReader')
class FromJSValue a where
    fromJSValue :: JSValue -> Maybe a
    fromJSValue j = runIdentity $ withJSValue j $ liftM fromJSValueM askJSValue
    fromJSValueM :: (JSValueContainer c , MonadReader c m) => m (Maybe a)
    fromJSValueM = liftM fromJSValue askJSValue

-- ---------------------------------------------------------------------------
-- Instances for basic datatypes
--

instance FromJSValue JSValue where
    fromJSValue = Just

instance FromJSValue String where
    fromJSValue (JSString string) = Just $ fromJSString string
    fromJSValue _ = Nothing

instance FromJSValue BS.ByteString where
    fromJSValue s = fmap BS.fromString (fromJSValue s)

instance FromJSValue Integer where
    fromJSValue (JSRational _ r) = Just $ numerator r
    fromJSValue _ = Nothing

instance FromJSValue Int where
    fromJSValue j = liftM fromIntegral (fromJSValue j :: Maybe Integer)

instance FromJSValue Bool where
    fromJSValue (JSBool v) = Just $ v
    fromJSValue _ = Nothing

instance (FromJSValue a) => FromJSValue [a] where
    fromJSValue (JSArray list) = let plist = map fromJSValue list
                                 in if (all isJust plist)
                                     then Just $ map fromJust plist
                                     else Nothing

    fromJSValue _ = Nothing

instance (FromJSValue a) => FromJSValue (Maybe a) where
    fromJSValue = join . fromJSValue


-- ----------------------------------------------------------------

-- | Getting JSON part of envirement
askJSValue :: (JSValueContainer c , MonadReader c m) => m JSValue
askJSValue = liftM getJSValue ask


-- | Reading the value that is on some field. With field if current JSON is not object
fromJSValueField ::  (JSValueContainer c , MonadReader c m, FromJSValue a) => String -> m (Maybe a)
fromJSValueField s = liftM fromObject askJSValue
    where
      fromObject (JSObject object) = join (fmap fromJSValue (lookup s $ fromJSObject object))
      fromObject _ = Nothing

-- | Version of 'fromJSValueField' for Base64 encoded data to  avoid memory leak.
fromJSValueFieldBase64 ::(JSValueContainer c , MonadReader c m) =>String -> m  (Maybe BS.ByteString)
fromJSValueFieldBase64 s =  liftM dc (fromJSValueField s)
    where dc s' = case (fmap BASE64.decode s') of
                            Just (Right r) -> Just r
                            _ -> Nothing

-- | Generalization of 'fromJSValueField'. Does not use 'FromJSValue' instances
fromJSValueFieldCustom :: (JSValueContainer c , MonadReader c m) =>  String -> m (Maybe a) -> m (Maybe a)
fromJSValueFieldCustom s digger = do
    mobj <- fromJSValueField s
    case mobj of
         Just obj -> local (setJSValue obj) (digger)
         Nothing -> return Nothing
         
-- | Runs parser on each element of underlaying json. Returns Just iff JSON is array.
fromJSValueCustomMany :: (JSValueContainer c , MonadReader c m) => m (Maybe a) -> m (Maybe [a])
fromJSValueCustomMany digger = fromJSValueCustomList (repeat digger)

-- | Generalization of 'fromJSValueCustomMany', where each element of array can have different parser.
fromJSValueCustomList :: (JSValueContainer c , MonadReader c m) => [m (Maybe a)] -> m (Maybe [a])
fromJSValueCustomList diggers = do
    mlist <- fromJSValueM
    case mlist of
         Nothing -> return Nothing
         Just list ->  runDiggers list diggers
    where
         runDiggers (j:js) (d:ds) = do
             mres <- local (setJSValue j) d
             case mres of
                 Just res -> do
                     mress <- runDiggers js ds
                     case mress of
                         Just ress -> return $ Just (res:ress)
                         _ -> return Nothing
                 _ -> return Nothing
         runDiggers _ _ = return $ Just []
         
-- ----------------------------------------------------------------
-- | Simple runner
withJSValue :: (Monad m) => JSValue -> ReaderT JSValue m a -> m a
withJSValue j a = runReaderT a j
