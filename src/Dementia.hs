module Dementia (
canary
, DementiaException(..)
, Character(..)
, designedCharacter
, randomCharacter
, verifyName
, verifySector
, verifyClone
) where

import Control.Exception
import Data.Char
import Data.Typeable

-- canary test function for verifying testing framework functionality

canary :: String
canary = "tweet"

-- library-level exception type

data DementiaException = TreasonousNameException
                       | TreasonousSectorException
                       | TreasonousCloneException
  deriving (Show, Typeable)

instance Exception DementiaException

-- "Character" data type

data Character = Character { name :: Name
                           , sector :: Sector
                           , clone :: Clone
                           } deriving (Show)

-- "Name" constrained String data type

type Name = String

verifyName :: Name -> Bool
verifyName n
  | True = True
  | otherwise = False

-- "Sector" constrained String data type

type Sector = String

verifySector :: Sector -> Bool
verifySector s
  | length s == 3 && all isUpper s = True
  | otherwise = False

-- "Clone" constrained Integer data type

type Clone = Integer

verifyClone :: Clone -> Bool
verifyClone c
  | c >= 1 = True
  | otherwise = False

-- smart constructors for Character

designedCharacter :: Name -> Sector -> Clone -> Character
designedCharacter n s c
  | not $ verifyName n   = throw TreasonousNameException
  | not $ verifySector s = throw TreasonousSectorException
  | not $ verifyClone c  = throw TreasonousCloneException
  | otherwise            = Character n s c

randomCharacter :: Character
randomCharacter = designedCharacter generateName generateSector generateClone
  where generateName = "Ralph"
        generateSector = "BRO"
        generateClone = 3
