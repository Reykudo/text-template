{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | This module Parserovides types and functions for parsing and reParseresenting text templates
module Data.Text.Template.Parse (
  showTemplate,
  parseTemplate,
  Template (..),
  Var (..),
  Lit (..),
  Frag (..),
  template,
)
where

import Control.Applicative (many, (<|>))
import Control.DeepSeq (NFData (rnf))
import Data.Attoparsec.Text
import Data.Function (on)
import Data.String (IsString)
import Data.Text qualified as T
import Data.Text.Builder.Linear (Builder, fromText)
import Data.Vector (Vector)
import Data.Vector qualified as V
import GHC.Exts (IsString (fromString))
import GHC.Generics (Generic)
import Prelude hiding (takeWhile)

-- -----------------------------------------------------------------------------

-- | ReParseresents a literal text fragment in the template
data Lit = Lit {text :: !T.Text, builder :: !Builder}
  deriving stock (Generic, Show)

instance IsString Lit where
  fromString s = Lit{text = T.pack s, builder = fromString s}

-- Custom equality instance for Lit, comparing only the 'text' field
instance Eq Lit where
  (==) = on (==) (.text)
  {-# INLINE (==) #-}

-- Custom ordering instance for Lit, comparing only the 'text' field
instance Ord Lit where
  compare = on compare (.text)
  {-# INLINE compare #-}

-- NFData instance for Lit, with no additional forcing needed
instance NFData Lit where
  rnf _ = ()
  {-# INLINE rnf #-}

-- | ReParseresents a variable in the template, with optional modifiers
data Var = Var {var :: !T.Text, modifiers :: !(Vector T.Text)}
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | A template fragment, either a literal or a variable
data Frag
  = LitF Lit
  | VarF Var
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | ReParseresents a complete template as a vector of fragments
newtype Template = Template {unTemplate :: Vector Frag}
  deriving newtype (Eq, Semigroup, Monoid)
  deriving stock (Show)

-- | Converts a Template to Text
showTemplate :: Template -> T.Text
showTemplate (Template fs) = T.concat . V.toList . fmap showFrag $ fs
{-# INLINEABLE showTemplate #-}

-- | Converts a Frag to Text, escaping '$' in literals
showFrag :: Frag -> T.Text
showFrag = \case
  (VarF (Var{var, modifiers})) -> T.concat ["${", T.intercalate ":" (var : V.toList modifiers), "}"]
  (LitF (Lit{text})) -> T.concatMap escape text
 where
  escape '$' = "$$" -- Escape '$' by doubling it
  escape c = T.singleton c
{-# INLINE showFrag #-}

-- -----------------------------------------------------------------------------

-- | p a literal fragment, handling escaped '$$' sequences
pLit :: Parser Lit
pLit = do
  text <- T.concat <$> many1 (escaped <|> regularText)
  pure $ Lit{text, builder = fromText text}
 where
  escaped = string "$$" >> pure "$" -- Convert '$$' to '$'
  regularText = T.singleton <$> notChar '$'

-- | p a variable name (letters, digits, or underscores)
pVarName :: Parser T.Text
pVarName = T.pack <$> many1 (letter <|> digit <|> char '_' <|> char '\'')

-- | p a modifier (anything except ':' or '}')
pModifier :: Parser T.Text
pModifier = T.pack <$> many1 (letter <|> digit <|> char '_' <|> char '\'')

-- = T.pack <$> many1 (notChar ':' <|> notChar '}')

-- | p a variable with optional modifiers
pVar :: Parser Var
pVar = do
  _ <- string "${"
  varName <- pVarName
  modifiers <- many (char ':' >> pModifier)
  _ <- char '}'
  pure $ Var varName $ V.fromList modifiers

-- | p a single fragment (either literal or variable)
pFrag :: Parser Frag
pFrag = (VarF <$> try pVar) <|> (LitF <$> pLit)

-- | p the entire template string into a list of fragments
pTemplate :: Parser Template
pTemplate = Template . V.fromList <$> many pFrag

parseTemplate :: T.Text -> Either String Template
parseTemplate = parseOnly pTemplate

template :: T.Text -> Template
template = either error id . parseTemplate