module JSONEncoder
(
  run,
  -- * Value
  Value,
  null,
  boolean,
  number_integral,
  string,
  object,
  array,
  nullable,
  -- * Object
  Object,
  field,
  -- * Array
  Array,
  homo,
)
where

import JSONEncoder.Prelude hiding (length, null)
import ByteString.TreeBuilder
import qualified JSONEncoder.Builders as Builders


run :: Value a -> a -> Builder
run (Value (Op producer)) input =
  producer input

-- * Value
-------------------------

newtype Value a =
  Value (Op Builder a)
  deriving (Contravariant, Divisible, Decidable)

null :: Value ()
null =
  Value $ Op $
  const "null"

boolean :: Value Bool
boolean =
  Value $ Op $
  \case
    True -> "true"
    False -> "false"

number_integral :: Integral a => Value a
number_integral =
  Value $ Op $
  fromString . show . toInteger

string :: Value Text
string =
  Value $ Op $
  Builders.stringLiteral

object :: Object a -> Value a
object (Object (Op sectionsProducer)) =
  Value $ Op $
    sectionsProducer >>>
    mappend (Builders.asciiChar '{') >>>
    flip mappend (Builders.asciiChar '}')

array :: Array a -> Value a
array (Array (Op sectionsProducer)) =
  Value $ Op $
    sectionsProducer >>>
    mappend (Builders.asciiChar '[') >>>
    flip mappend (Builders.asciiChar ']')

nullable :: Value a -> Value (Maybe a)
nullable =
  choose (maybe (Left ()) Right) null


-- * Object
-------------------------

newtype Object a =
  Object (Op Builder a)
  deriving (Contravariant, Divisible, Decidable)

instance Monoid (Object a) where
  mempty =
    Object (Op (const mempty))
  mappend (Object (Op producer1)) (Object (Op producer2)) =
    Object (Op (Builders.appendWithIncut (Builders.asciiChar ',') <$> producer1 <*> producer2))

field :: Text -> Value a -> Object a
field name (Value (Op producer)) =
  Object $ Op $ 
    producer >>>
    mappend (Builders.asciiChar ':') >>>
    mappend (Builders.stringLiteral name)


-- * Array
-------------------------

newtype Array a =
  Array (Op Builder a)
  deriving (Contravariant)

homo :: (forall a. (a -> b -> a) -> a -> c -> a) -> Value b -> Array c
homo foldl (Value (Op producer)) =
  Array (Op arrayProducer)
  where
    arrayProducer =
      foldl step mempty
      where
        step acc =
          Builders.appendWithIncut (Builders.asciiChar ',') acc .
          producer
