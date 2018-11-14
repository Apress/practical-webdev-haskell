module Domain.Validation where
  
import ClassyPrelude
import Text.Regex.PCRE.Heavy

type ErrMsg = Text
type Validation a = a -> Maybe ErrMsg

validate :: (a -> b) -> [Validation a] -> a -> Either [ErrMsg] b
validate constructor validations val = 
  case concatMap (\f -> maybeToList $ f val) validations of
    []    -> Right $ constructor val
    errs  -> Left errs

rangeBetween :: (Ord a) => a -> a -> ErrMsg -> Validation a
rangeBetween minRange maxRange msg val =
  if val >= minRange && val <= maxRange then Nothing else Just msg

lengthBetween :: (MonoFoldable a) => Int -> Int -> ErrMsg -> Validation a
lengthBetween minLen maxLen msg val =
  rangeBetween minLen maxLen msg (length val)

regexMatches :: Regex -> ErrMsg -> Validation Text
regexMatches regex msg val = 
  if val =~ regex then Nothing else Just msg
