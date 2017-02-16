{-# LANGUAGE DataKinds #-}

-- | Build backend-agnostic columnar encodings that can be 
--   used to visualize tabular data.
module Colonnade
  ( -- * Example
    -- $setup
    -- * Types
    Colonnade
  , Headed
  , Headless
    -- * Create
  , headed
  , headless
  , singleton
    -- * Transform
  , mapHeaderContent
  , fromMaybe
  , columns
  , bool
  , replaceWhen
  , modifyWhen
    -- * Cornice
    -- ** Types
  , Cornice
  , Pillar(..)
  , Fascia(..)
    -- ** Create
  , cap
  , recap
    -- * Ascii Table
  , ascii
  ) where

import Colonnade.Internal
import Data.Foldable
import Data.Monoid (Endo(..))
import Control.Monad
import qualified Colonnade.Encode as Encode
import qualified Colonnade.Cornice.Encode as CE
import qualified Data.Bool
import qualified Data.Maybe
import qualified Data.List as List
import qualified Data.Vector as Vector

-- $setup
--
-- First, let\'s bring in some neccessary imports that will be
-- used for the remainder of the examples in the docs:
--
-- >>> import Data.Monoid (mconcat,(<>))
-- >>> import Data.Profunctor (lmap)
--
-- The data types we wish to encode are:
--
-- >>> data Color = Red | Green | Blue deriving (Show,Eq)
-- >>> data Person = Person { name :: String, age :: Int }
-- >>> data House = House { color :: Color, price :: Int }
--
-- One potential columnar encoding of a @Person@ would be:
--
-- >>> :{
-- let colPerson :: Colonnade Headed Person String
--     colPerson = mconcat
--       [ headed "Name" name
--       , headed "Age" (show . age)
--       ]
-- :}
--
-- The type signature on @colPerson@ is not neccessary
-- but is included for clarity. We can feed data into this encoding
-- to build a table:
--
-- >>> let people = [Person "David" 63, Person "Ava" 34, Person "Sonia" 12]
-- >>> putStr (ascii colPerson people)
-- +-------+-----+
-- | Name  | Age |
-- +-------+-----+
-- | David | 63  |
-- | Ava   | 34  |
-- | Sonia | 12  |
-- +-------+-----+
--
-- Similarly, we can build a table of houses with:
--
-- >>> let showDollar = (('$':) . show) :: Int -> String
-- >>> :{
-- let colHouse :: Colonnade Headed House String
--     colHouse = mconcat
--       [ headed "Color" (show . color)
--       , headed "Price" (showDollar . price)
--       ]
-- :}
--
-- >>> let houses = [House Green 170000, House Blue 115000, House Green 150000]
-- >>> putStr (ascii colHouse houses)
-- +-------+---------+
-- | Color | Price   |
-- +-------+---------+
-- | Green | $170000 |
-- | Blue  | $115000 |
-- | Green | $150000 |
-- +-------+---------+


-- | A single column with a header.
headed :: c -> (a -> c) -> Colonnade Headed a c
headed h = singleton (Headed h)

-- | A single column without a header.
headless :: (a -> c) -> Colonnade Headless a c
headless = singleton Headless

-- | A single column with any kind of header. This is not typically needed.
singleton :: h c -> (a -> c) -> Colonnade h a c
singleton h = Colonnade . Vector.singleton . OneColonnade h

-- | Map over the content in the header. This is similar performing 'fmap'
--   on a 'Colonnade' except that the body content is unaffected.
mapHeaderContent :: Functor h => (c -> c) -> Colonnade h a c -> Colonnade h a c
mapHeaderContent f (Colonnade v) = 
  Colonnade (Vector.map (\(OneColonnade h e) -> OneColonnade (fmap f h) e) v)

-- | Lift a column over a 'Maybe'. For example, if some people
--   have houses and some do not, the data that pairs them together
--   could be represented as:
--
-- >>> :{
-- let owners :: [(Person,Maybe House)]
--     owners =
--       [ (Person "Jordan" 18, Nothing)
--       , (Person "Ruth" 25, Just (House Red 125000))
--       , (Person "Sonia" 12, Just (House Green 145000))
--       ]
-- :}
--
-- The column encodings defined earlier can be reused with
-- the help of 'fromMaybe':
--
-- >>> :{
-- let colOwners :: Colonnade Headed (Person,Maybe House) String
--     colOwners = mconcat
--       [ lmap fst colPerson
--       , lmap snd (fromMaybe "" colHouse)
--       ]
-- :}
--
-- >>> putStr (ascii colOwners owners)
-- +--------+-----+-------+---------+
-- | Name   | Age | Color | Price   |
-- +--------+-----+-------+---------+
-- | Jordan | 18  |       |         |
-- | Ruth   | 25  | Red   | $125000 |
-- | Sonia  | 12  | Green | $145000 |
-- +--------+-----+-------+---------+
fromMaybe :: c -> Colonnade f a c -> Colonnade f (Maybe a) c
fromMaybe c (Colonnade v) = Colonnade $ flip Vector.map v $
  \(OneColonnade h encode) -> OneColonnade h (maybe c encode)

-- | Convert a collection of @b@ values into a columnar encoding of
--   the same size. Suppose we decide to show a house\'s color
--   by putting a check mark in the column corresponding to
--   the color instead of by writing out the name of the color:
--
-- >>> let allColors = [Red,Green,Blue]
-- >>> let encColor = columns (\c1 c2 -> if c1 == c2 then "✓" else "") (Headed . show) allColors
-- >>> :t encColor
-- encColor :: Colonnade Headed Color [Char]
-- >>> let encHouse = headed "Price" (showDollar . price) <> lmap color encColor
-- >>> :t encHouse
-- encHouse :: Colonnade Headed House [Char]
-- >>> putStr (ascii encHouse houses)
-- +---------+-----+-------+------+
-- | Price   | Red | Green | Blue |
-- +---------+-----+-------+------+
-- | $170000 |     | ✓     |      |
-- | $115000 |     |       | ✓    |
-- | $150000 |     | ✓     |      |
-- +---------+-----+-------+------+
columns :: Foldable g
  => (b -> a -> c) -- ^ Cell content function
  -> (b -> f c) -- ^ Header content function
  -> g b -- ^ Basis for column encodings
  -> Colonnade f a c
columns getCell getHeader = id
  . Colonnade
  . Vector.map (\b -> OneColonnade (getHeader b) (getCell b))
  . Vector.fromList
  . toList

bool ::
     f c -- ^ Heading
  -> (a -> Bool) -- ^ Predicate
  -> (a -> c) -- ^ Contents when predicate is false
  -> (a -> c) -- ^ Contents when predicate is true
  -> Colonnade f a c
bool h p onTrue onFalse = singleton h (Data.Bool.bool <$> onFalse <*> onTrue <*> p)

-- | Modify the contents of cells in rows whose values satisfy the
--   given predicate. Header content is unaffected. With an HTML backend, 
--   this can be used to strikethrough the contents of cells with data that is
--   considered invalid.
modifyWhen ::
     (c -> c) -- ^ Content change
  -> (a -> Bool) -- ^ Row predicate
  -> Colonnade f a c -- ^ Original 'Colonnade'
  -> Colonnade f a c
modifyWhen changeContent p (Colonnade v) = Colonnade
  ( Vector.map
    (\(OneColonnade h encode) -> OneColonnade h $ \a ->
      if p a then changeContent (encode a) else encode a
    ) v
  )

-- | Replace the contents of cells in rows whose values satisfy the
--   given predicate. Header content is unaffected.
replaceWhen ::
     c -- ^ New content
  -> (a -> Bool) -- ^ Row predicate
  -> Colonnade f a c -- ^ Original 'Colonnade'
  -> Colonnade f a c
replaceWhen newContent p (Colonnade v) = Colonnade
  ( Vector.map
    (\(OneColonnade h encode) -> OneColonnade h $ \a ->
      if p a then newContent else encode a
    ) v
  )

-- | Augment a 'Colonnade' with a header spans over all of the
--   existing headers. This is best demonstrated by example. 
--   Let\'s consider how we might encode a pairing of the people 
--   and houses from the initial example:
--   
--   >>> let personHomePairs = zip people houses
--   >>> let colPersonFst = lmap fst colPerson
--   >>> let colHouseSnd = lmap snd colHouse
--   >>> putStr (ascii (colPersonFst <> colHouseSnd) personHomePairs)
--   +-------+-----+-------+---------+
--   | Name  | Age | Color | Price   |
--   +-------+-----+-------+---------+
--   | David | 63  | Green | $170000 |
--   | Ava   | 34  | Blue  | $115000 |
--   | Sonia | 12  | Green | $150000 |
--   +-------+-----+-------+---------+
--   
--   This tabular encoding leaves something to be desired. The heading
--   not indicate that the name and age refer to a person and that
--   the color and price refer to a house. Without reaching for 'Cornice',
--   we can still improve this situation with 'mapHeaderContent':
--
--   >>> let colPersonFst' = mapHeaderContent ("Person " ++) colPersonFst
--   >>> let colHouseSnd' = mapHeaderContent ("House " ++) colHouseSnd
--   >>> putStr (ascii (colPersonFst' <> colHouseSnd') personHomePairs)
--   +-------------+------------+-------------+-------------+
--   | Person Name | Person Age | House Color | House Price |
--   +-------------+------------+-------------+-------------+
--   | David       | 63         | Green       | $170000     |
--   | Ava         | 34         | Blue        | $115000     |
--   | Sonia       | 12         | Green       | $150000     |
--   +-------------+------------+-------------+-------------+
--
--   This is much better, but for longer tables, the redundancy
--   of prefixing many column headers can become annoying. The solution
--   that a 'Cornice' offers is to nest headers:
--   
--   >>> let cor = mconcat [cap "Person" colPersonFst, cap "House" colHouseSnd]
--   >>> :t cor
--   cor :: Cornice ('Cap 'Base) (Person, House) [Char]
--   >>> putStr (asciiCapped cor personHomePairs)
--   foo
--   
cap :: c -> Colonnade Headed a c -> Cornice (Cap Base) a c
cap h = CorniceCap . Vector.singleton . OneCornice h . CorniceBase

recap :: c -> Cornice p a c -> Cornice (Cap p) a c
recap h cor = CorniceCap (Vector.singleton (OneCornice h cor))

asciiCapped :: Foldable f
  => Cornice p a String -- ^ columnar encoding
  -> f a -- ^ rows
  -> String
asciiCapped cor xs =
  let annCor = CE.annotateFinely (\x y -> x + y + 3) id 
        List.length xs cor
   in CE.headersMonoidal "|"
        (Right (\s -> "|" ++ s ++ "\n")) 
        (\sz c -> " " ++ rightPad sz ' ' c ++ " |") annCor
      

-- | Render a collection of rows as an ascii table. The table\'s columns are
-- specified by the given 'Colonnade'. This implementation is inefficient and
-- does not provide any wrapping behavior. It is provided so that users can
-- try out @colonnade@ in ghci and so that @doctest@ can verify example
-- code in the haddocks.
ascii :: Foldable f
  => Colonnade Headed a String -- ^ columnar encoding
  -> f a -- ^ rows
  -> String
ascii col xs = 
  let sizedCol = Encode.sizeColumns List.length xs col
      divider = concat
        [ "+" 
        , Encode.headerMonoidalFull sizedCol 
             (\(Sized sz _) -> hyphens (sz + 2) ++ "+")
        , "\n"
        ]
      rowContents = foldMap
        (\x -> concat
           [ "|"
           , Encode.rowMonoidalHeader 
               sizedCol
               (\(Sized sz _) c -> " " ++ rightPad sz ' ' c ++ " |")
               x
           , "\n"
           ]
        ) xs
   in List.concat
      [ divider
      , concat
         [ "|"
         , Encode.headerMonoidalFull sizedCol
             (\(Sized s (Headed h)) -> " " ++ rightPad s ' ' h ++ " |")
         , "\n"
         ]
      , divider
      , rowContents
      , divider
      ]
      
hyphens :: Int -> String
hyphens n = List.replicate n '-'

rightPad :: Int -> a -> [a] -> [a]
rightPad m a xs = take m $ xs ++ repeat a

-- data Company = Company String String Int
-- 
-- data Company = Company
--   { companyName :: String
--   , companyCountry :: String
--   , companyValue :: Int
--   } deriving (Show)
-- 
-- myCompanies :: [Company]
-- myCompanies =
--   [ Company "eCommHub" "United States" 50
--   , Company "Layer 3 Communications" "United States" 10000000
--   , Company "Microsoft" "England" 500000000
--   ]





