{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, OverlappingInstances, OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module HereDoc (heredoc) where
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Data.Attoparsec.Text
import qualified Data.Text as T
import Data.Generics
import Control.Applicative
import Control.Monad
import Data.Maybe

heredoc :: QuasiQuoter
heredoc = QuasiQuoter { 
          quoteExp = parseExp
        , quotePat = undefined 
        , quoteType = undefined 
        , quoteDec = undefined 
}

data HereDoc = HDString String
             | HDQuote String
             | HDList [HereDoc]
    deriving (Show, Eq, Data, Typeable)

--------------------------------------------------------------------------------

hdString = HDString . T.unpack <$> takeWhile1 (notInClass "${")
hdQuote = HDQuote . T.unpack <$> (curlyBraced antiQuoteSymbol <|> antiQuoteSymbol)
notHdQuote = HDString . T.unpack <$> (string "$" <|> string "{")
antiQuoteSymbol = string "$" *> (liftM T.pack $ many1 (letter <|> digit <|> char '_'))
curlyBraced p = "{" .*> p <*. "}"
parserHereDoc = many (try hdQuote <|> try notHdQuote <|> try hdString)

--------------------------------------------------------------------------------

parseQa :: (HereDoc -> Q a) -> String -> Q a
parseQa heredocToA str =
    case parseOnly parserHereDoc (T.pack str) of
        Right hd -> heredocToA $ HDList hd
        Left _ -> error "err"

parseExp :: String -> ExpQ
parseExp = parseQa (dataToExpQ $ Nothing `mkQ` antiQuoteE)

antiQuoteE :: HereDoc -> Maybe ExpQ
antiQuoteE (HDQuote nm) = Just $ {-appE (varE 'show)-} (varE $ mkName nm)
antiQuoteE (HDString nm) = Just $ litE (stringL nm)
antiQuoteE (HDList hds) = Just $ appE (varE 'concat) (listE $ mapMaybe antiQuoteE hds)
