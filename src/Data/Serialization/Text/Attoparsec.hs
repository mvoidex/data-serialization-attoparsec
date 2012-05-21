{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}

module Data.Serialization.Text.Attoparsec (
    Atto,
    atto
    ) where

import Control.Applicative
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Attoparsec.ByteString
import Data.Serialization.Deserialize
import Data.Serialization.Serializable

newtype Atto a = Atto {
    attoParsec :: Parser a }
        deriving (Functor, Applicative, Alternative, Monad)

-- | Attoparsec deserialize
atto :: Parser a -> Deserialize Atto a
atto p = Deserialize (Atto p)

instance Deserialization Atto ByteString where
    runDeserialization (Atto p) s = result $ parse p s where
        result (Fail _ _ s) = Left s
        result (Partial f) = result $ f B.empty
        result (Done _ r) = Right r
    deserializationEof _ = Atto endOfInput
    deserializeTail = Atto takeByteString
