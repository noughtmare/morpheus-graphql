{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Case.Shopify.Test where

import Data.Coerce
import Data.Morpheus.Client
  ( DecodeScalar (..),
    EncodeScalar (..),
    ScalarValue (String),
    defineByIntrospectionFile,
    gql,
  )
import Data.Morpheus.Types.ID (ID)
import Data.Text
import Prelude

pvs :: Coercible Text a => ScalarValue -> Either Text a
pvs (String x) = pure $ coerce x
pvs x = fail $ "Expecting ScalarValue.String but received: " <> show x

newtype DateTime = DateTime Text deriving (Show, Eq)

instance DecodeScalar DateTime where
  decodeScalar = pvs

instance EncodeScalar DateTime where
  encodeScalar = String . coerce

newtype Money = Money Text deriving (Show, Eq)

instance DecodeScalar Money where
  decodeScalar = pvs

instance EncodeScalar Money where
  encodeScalar = String . coerce

newtype Decimal = Decimal Text deriving (Show, Eq)

instance DecodeScalar Decimal where
  decodeScalar = pvs

instance EncodeScalar Decimal where
  encodeScalar = String . coerce

defineByIntrospectionFile
  "test/Case/Shopify/schema.json"
  [gql| mutation SomCrzyuName ($input: ProductInput!) {
          productCreate(input: $input, media: []) {}
        } 
  |]
