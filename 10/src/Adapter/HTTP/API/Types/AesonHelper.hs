module Adapter.HTTP.API.Types.AesonHelper where

import ClassyPrelude
import Data.Aeson.TH
import Data.Aeson.Types
import Language.Haskell.TH.Syntax

withSmartConstructor :: (a -> Either [Text] b) -> a -> Parser b
withSmartConstructor constructor a =
  case constructor a of
    Left errs -> fail $ intercalate ". " . map unpack $ errs
    Right val -> return val

deriveJSONRecord :: Name -> Q [Dec]
deriveJSONRecord record =
  let lowerCaseFirst (y:ys) = toLower [y] <> ys 
      lowerCaseFirst "" = ""
      structName = nameBase record
      opts = defaultOptions
              { fieldLabelModifier = lowerCaseFirst . drop (length structName)
              }
  in deriveJSON opts record

deriveJSONSumType :: Name -> Q [Dec]
deriveJSONSumType record =
  let structName = nameBase record
      opts = defaultOptions
              { constructorTagModifier = drop (length structName)
              , tagSingleConstructors = True
              }
  in deriveJSON opts record

deriveToJSONUnwrap :: Name -> Q [Dec]
deriveToJSONUnwrap =
  let opts = defaultOptions { unwrapUnaryRecords = True }
  in deriveToJSON opts