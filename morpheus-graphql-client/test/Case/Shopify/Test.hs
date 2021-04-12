module Case.Shopify.Test where

import Data.Coerce
import Data.Morpheus.Client (GQLScalar (parseValue, serialize), ScalarValue (String), defineByIntrospectionFile, gql)
import Data.Morpheus.Types.ID (ID)
import Data.Text
import Prelude

stringParseFail x = fail $ "Expecting ScalarValue.String but received: " <> show x

pvs :: Coercible Text a => ScalarValue -> Either Text a
pvs = \case
  String x -> pure $ coerce x
  x -> stringParseFail x

newtype DateTime = DateTime Text deriving (Show, Eq)

instance GQLScalar DateTime where
  parseValue = pvs
  serialize = String . coerce

newtype Money = Money Text deriving (Show, Eq)

instance GQLScalar Money where
  parseValue = pvs
  serialize = String . coerce

newtype Decimal = Decimal Text deriving (Show, Eq)

instance GQLScalar Decimal where
  parseValue = pvs
  serialize = String . coerce

defineByIntrospectionFile
  "./morpheus-graphql-client/test/Case/Shopify/schema.json"
  [gql| mutation SomCrzyuName ($input: ProductInput!) {
          productCreate(input: $input, media: []) {}
        } 
  |]
