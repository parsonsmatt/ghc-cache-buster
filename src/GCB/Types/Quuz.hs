{-# language DeriveLift #-}
{-# language TemplateHaskell, QuasiQuotes #-}

module GCB.Types.Quuz
    ( Quuz
    , mkQuuz
    , compileQuuz
    , getQuuz
    , blargh
    ) where

import GCB.Prelude

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote

-- asdf

data Quuz = Quuz String
    deriving Lift

mkQuuz :: String -> Either String Quuz
mkQuuz str =
    if all isLower str
        then Right $ Quuz str
        else Left $ "Expected all lowercase characters for Quuz, got: " <> str

compileQuuz :: QuasiQuoter
compileQuuz =
    QuasiQuoter
        { quoteExp = \str -> do
            case mkQuuz str of
                Left err -> do
                    fail err
                Right a -> do
                    lift a
        , quotePat = \str ->
            error "Can't use in pattern"
        , quoteType = error "can't use in type"
        , quoteDec = error "can't use in dec"
        }

getQuuz :: Quuz -> String
getQuuz (Quuz a) = a

blargh :: Q [Dec]
blargh = pure []
