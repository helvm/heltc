module HelVM.HelTC.Calculators.Legacy.Parsers.ZotParser (
  parseZot,
) where

import           HelVM.HelTC.Calculators.Lambda.Lambda

import           HelVM.HelTC.Calculators.Lambda.Parsers.UnLambdaParser

import           HelVM.HelTC.Calculators.Legacy.Parse

import           Data.Char

import qualified Relude.Unsafe                                         as Unsafe

import qualified Data.Text                                             as Text

parseZot :: Text -> Lambda
parseZot = parseUnLambda . convert

convert :: Text -> Text
convert = toText . fst . Unsafe.head . (zotParser >*> eof `build` fst) . toString . filterNonSeparator

filterNonSeparator :: Text -> Text
filterNonSeparator = Text.filter nonSeparator

nonSeparator :: Char -> Bool
nonSeparator = not . isSeparator

zotParser :: Parse Char String
--zotParser = sParser `alt` kParser `alt` iParser `alt` applyParser
zotParser = applyParser `alt` sParser `alt` kParser `alt` iParser

sParser:: Parse Char String
sParser = tokens "101010100" `build` const "s"

kParser :: Parse Char String
kParser = tokens "1010100" `build` const "k"

iParser :: Parse Char String
iParser = tokens "100" `build` const "i"

applyParser :: Parse Char String
applyParser = token '1' >*> zotParser >*> zotParser `build` \(_, (ski1, ski2)) -> '`' : ski1 <> ski2
