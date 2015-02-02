{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}
module Web.Neptune.Tools.Vault (
	-- * Constructing Vaults
	  MkVault
	, mkVault
	, newKey
	, (===)
	-- * Quick Datum
	, QDatum(..)
	, qRoute
	, qDatum
	, qDatumOr
	, qDatum_f
	, qDatumOr_f
	, qDatum_ff
	) where

import System.IO.Unsafe

import Web.Neptune.Core
import Web.Neptune.Route
import Web.Neptune.Escape
import Web.Neptune.Tools.Encoding

import qualified Data.Text as T
import qualified Data.Text.Read as T

import qualified Data.Map as Map
import qualified Data.Vault.Lazy as Vault
import Control.Monad.State


-- |Accumulation monad to easily build a Vault.
newtype MkVault a = MkVault { unMkVault :: State Vault a }
    deriving(Functor, Applicative, Monad)
mkVault :: MkVault () -> Vault
mkVault = flip execState Vault.empty . unMkVault

-- |Create a new 'Key'. Beware: performs unsafe IO.
newKey :: Key a
newKey = unsafePerformIO Vault.newKey

-- |Add a 'Key'-value pair to a 'Vault' using the 'MkVault' monad.
(===) :: Key a -> a -> MkVault ()
k === v = MkVault $ modify $ Vault.insert k v


{- Quick Data -}
_quickKey :: Key (Map Text Text)
_quickKey = newKey

-- |A 'Route' that uses the quick-data store.
qRoute :: Text -> Route
qRoute name = R fore back
    where
    fore = do
        [captured] <- consume 1
        qData <- datumOr Map.empty _quickKey
        let qData' = Map.insert name captured qData
        setDatum _quickKey qData'
    back = do
        m_datum <- Map.lookup name <$> datumOr Map.empty _quickKey
        case m_datum of 
            Just datum -> create datum
            Nothing -> Reverse . lift . lift $ Nothing

-- |Obtain a quick-datum.
--  If it does not exist, then this returns 'Nothing'.
--  If it cannot be parsed to the appropriate type, then return a 'Left' with an error message.
qDatum :: (DatumMonad m, QDatum a) => Text -> m (Maybe (Either Text a))
qDatum name = do
    m_datum <- Map.lookup name `liftM` datumOr Map.empty _quickKey
    case m_datum of
        Nothing -> return Nothing
        Just datum -> return . Just $ case toQDatum datum of
            Left err -> Left err
            Right val -> Right val

-- |As 'qDatum', but use the supplied default if the quick datum does not exist.
qDatumOr :: (DatumMonad m, QDatum a) => a -> Text -> m (Either Text a)
qDatumOr def name = fromMaybe (Right def) `liftM` qDatum name

-- |As 'qDatum', but raise an 'InternalError' if the quick datum cannot be parsed.
qDatum_f :: (ResultMonad m, DatumMonad m, QDatum a) => Text -> m (Maybe a)
qDatum_f name = do
    me_x <- qDatum name
    case me_x of
        Nothing -> return Nothing
        Just e_x -> case e_x of
            Left err -> internalError $ "Error parsing qDatum " <> name <> ": " <> err
            Right x -> return $ Just x

-- |As 'qDatum', but use the supplied default if the quick datum does not exist,
--  and raise an 'InternalError' if it cannot be parsed.
qDatumOr_f :: (ResultMonad m, DatumMonad m, QDatum a) => a -> Text -> m a
qDatumOr_f def name = do
    me_x <- qDatum name
    case me_x of
        Nothing -> return def
        Just e_x -> case e_x of
            Left err -> internalError $ "Error parsing qDatum " <> name <> ": " <> err
            Right x -> return x

-- |As 'qDatum', but raise an 'InternalError' if the quick datum does not exist,
--  or cannot be parsed.
qDatum_ff :: (ResultMonad m, DatumMonad m, QDatum a) => Text -> m a
qDatum_ff name = do
    me_x <- qDatum name
    case me_x of
        Nothing -> internalError $ "Error: no such qDatum: " <> name
        Just e_x -> case e_x of
            Left err -> internalError $ "Error parsing qDatum " <> name <> ": " <> err
            Right x -> return x


{-| Types which can be parsed during retrieval from the quick-data store. -}
class QDatum a where
    -- |Parse the stored quick datum into an appropriate type or
    --  else return an error message.
    toQDatum :: Text -> Either Text a
instance QDatum Text where
    toQDatum = Right
instance QDatum LText where
    toQDatum = Right . fromStrictT
instance QDatum String where
    toQDatum = Right . T.unpack
instance QDatum Int where
    toQDatum x = case T.decimal x of
        Right (n, "") -> Right n
        _ -> Left $ "could not parse integer (" <> x <> ")"
instance QDatum Integer where
    toQDatum x = case T.decimal x of
        Right (n, "") -> Right n
        _ -> Left $ "could not parse integer (" <> x <> ")"