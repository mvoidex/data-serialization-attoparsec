{-# LANGUAGE TypeFamilies, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving, FunctionalDependencies #-}

module Data.Serialization.Text.Attoparsec (
    Atto,
    atto
    ) where

import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Text.Lazy as LT
import Data.Attoparsec.Types
import qualified Data.Attoparsec.ByteString as AB
import qualified Data.Attoparsec.ByteString.Lazy as ALB
import qualified Data.Attoparsec.Text as AT
import qualified Data.Attoparsec.Text.Lazy as ALT
import Data.Serialization.Deserialize
import Data.Serialization.Serializable
import Data.Monoid

newtype Atto s a = Atto {
    attoParsec :: Parser s a }
        deriving (Functor, Applicative, Alternative, Monad)

-- | Attoparsec deserialize
atto :: Parser s a -> Deserialize (Atto s) a
atto p = Deserialize (Atto p)

data AttoResult r =
    AttoFail String |
    AttoDone r

class Parseable s c | s -> c where
    attoParse :: Parser c a -> s -> AttoResult a
    attoEOF :: Hint s -> Parser c ()
    attoRest :: Parser c s

iresult :: (Monoid t) => IResult t r -> AttoResult r
iresult (Fail _ _ s) = AttoFail s
iresult (Partial f) = iresult $ f mempty
iresult (Done _ r) = AttoDone r

instance Parseable B.ByteString B.ByteString where
    attoParse p i = iresult $ AB.parse p i
    attoEOF _ = AB.endOfInput
    attoRest = AB.takeByteString

instance Parseable LB.ByteString B.ByteString where
    attoParse p i = case ALB.parse p i of
        (ALB.Fail _ _ s) -> AttoFail s
        (ALB.Done _ r) -> AttoDone r
    attoEOF _ = AB.endOfInput
    attoRest = AB.takeLazyByteString

instance Parseable T.Text T.Text where
    attoParse i p = iresult $ AT.parse i p
    attoEOF _ = AT.endOfInput
    attoRest = AT.takeText

instance Parseable LT.Text T.Text where
    attoParse p i = case ALT.parse p i of
        (ALT.Fail _ _ s) -> AttoFail s
        (ALT.Done _ r) -> AttoDone r
    attoEOF _ = AT.endOfInput
    attoRest = AT.takeLazyText

instance (Monoid c, Parseable s c) => Deserialization (Atto c) s where
    runDeserialization (Atto p) s = result $ attoParse p s where
        result (AttoFail s) = Left s
        result (AttoDone r) = Right r
    deserializationEof h = Atto $ attoEOF h
    deserializeTail = Atto attoRest
