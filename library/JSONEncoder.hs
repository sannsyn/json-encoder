module JSONEncoder
(
  run,
  -- * Value
  Value,
  null,
  boolean,
  number_integral,
  number_scientific,
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
  hetero,
  -- * Hetero
  Hetero,
  element,
)
where

import JSONEncoder.Prelude hiding (length, null)
import ByteString.TreeBuilder
import qualified JSONEncoder.Builders as Builders
import qualified Data.Scientific


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

number_scientific :: Value Data.Scientific.Scientific
number_scientific =
  Value $ Op $
  fromString . show

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
  deriving (Contravariant)

instance Divisible Object where
  conquer =
    mempty
  divide divisor (Object (Op producer1)) (Object (Op producer2)) =
    Object $ Op $ 
      divisor >>> \(input1, input2) ->
      Builders.appendWithIncut (Builders.asciiChar ',') (producer1 input1) (producer2 input2)

instance Decidable Object where
  lose f =
    Object (lose f)
  choose f (Object op1) (Object op2) =
    Object (choose f op1 op2)

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

-- |
-- A homogenous array.
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

-- |
-- A heterogenous array encoder.
hetero :: Hetero a -> Array a
hetero (Hetero op) =
  Array op


-- * Hetero
-------------------------

newtype Hetero a =
  Hetero (Op Builder a)
  deriving (Contravariant)

instance Divisible Hetero where
  conquer =
    mempty
  divide divisor (Hetero (Op producer1)) (Hetero (Op producer2)) =
    Hetero $ Op $ 
      divisor >>> \(input1, input2) ->
      Builders.appendWithIncut (Builders.asciiChar ',') (producer1 input1) (producer2 input2)

instance Decidable Hetero where
  lose f =
    Hetero (lose f)
  choose f (Hetero op1) (Hetero op2) =
    Hetero (choose f op1 op2)

instance Monoid (Hetero a) where
  mempty =
    Hetero (Op (const mempty))
  mappend (Hetero (Op producer1)) (Hetero (Op producer2)) =
    Hetero (Op (Builders.appendWithIncut (Builders.asciiChar ',') <$> producer1 <*> producer2))

element :: Value a -> Hetero a
element (Value op) =
  Hetero op

